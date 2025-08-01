/-
Copyright (c) 2025 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Sofia Rodrigues
-/
import Std.Internal.Async.Select
import Std.Data.TreeMap
import Std

namespace MiniRedis
open Std.Internal.IO.Async

inductive Consumer (α : Type) where
  | normal (promise : IO.Promise α)
  | select (finished : Waiter α)

def Consumer.resolve (c : Consumer α) (x : α) : BaseIO Bool := do
  match c with
  | .normal promise =>
    promise.resolve x
    return true
  | .select waiter =>
    let lose := return false
    let win promise := do
      promise.resolve (.ok x)
      return true
    waiter.race lose win

structure Notify.State where
  consumers : Std.Queue (Consumer Unit)

/--
A notify is a synchronization primitive that allows multiple consumers to wait
until notify is called.
-/
structure Notify where
  state : Std.Mutex Notify.State

namespace Notify

/--
Create a new notify.
-/
def new : BaseIO Notify := do
  return { state := ← Std.Mutex.new { consumers := ∅ } }

/--
Notify all currently waiting consumers.
-/
def notify (x : Notify) : BaseIO Unit := do
  x.state.atomically do
    let mut st ← get

    let mut remainingConsumers := st.consumers
    st := { st with consumers := ∅ }

    while true do
      if let some (consumer, rest) := remainingConsumers.dequeue? then
        remainingConsumers := rest
        discard <| consumer.resolve ()
      else
        break

    set st

/--
Notify exactly one waiting consumer (if any). Returns true if a consumer
was notified, false if no consumers were waiting.
-/
def notifyOne (x : Notify) : BaseIO Bool := do
  x.state.atomically do
    let mut st ← get

    if let some (consumer, rest) := st.consumers.dequeue? then
      st := { st with consumers := rest }
      set st
      consumer.resolve ()
    else
      return false

/--
Wait to be notified. Returns a task that completes when notify is called.
Note: if notify was called before wait, this will wait for the next notify call.
-/
def wait (x : Notify) : BaseIO (AsyncTask Unit) := do
  x.state.atomically do
    let promise ← IO.Promise.new
    modify fun st => { st with consumers := st.consumers.enqueue (.normal promise) }
    return promise.result?.map fun
      | some res => .ok res
      | none => .error (.userError "notify dropped")

/--
Creates a selector that waits for notifications
-/
def selector (notify : Notify) : Selector Unit := {
  tryFn := do
    return none

  registerFn := fun waiter => do
    notify.state.atomically do
      modify fun st => { st with consumers := st.consumers.enqueue (.select waiter) }

  unregisterFn := do
    notify.state.atomically do
      let st ← get

      let consumers ← st.consumers.filterM fun
        | .normal _ => return true
        | .select waiter => return !(← waiter.checkFinished)

      set { st with consumers }
}

end Notify
end MiniRedis
