/-
Copyright (c) 2025 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Sofia Rodrigues
-/
import Std.Internal.Async.Select
import Std.Data.TreeMap
import MiniRedis.Util.Notify
import Std

namespace MiniRedis
open Std.Internal.IO.Async

private structure Signal.State where
  notify : Notify
  signaled : Bool

/--
A signal is a type that combines a notify with a boolean state.
-/
structure Signal where
  state : Std.Mutex Signal.State

namespace Signal

/--
Create a new signal with initial signaled state.
-/
def new (initialState : Bool := false) : BaseIO Signal := do
  let notify ← Notify.new
  return { state := ← Std.Mutex.new { notify, signaled := initialState } }

/--
Signal all waiting consumers and set the signaled state to true.
-/
def signal (x : Signal) : BaseIO Unit := do
  x.state.atomically do
    let st ← get
    set { st with signaled := true }
    st.notify.notify

/--
Reset the signal to unsignaled state.
-/
def reset (x : Signal) : BaseIO Unit := do
  x.state.atomically do
    modify fun st => { st with signaled := false }

/--
Reset the signal to unsignaled state.
-/
def removeWaiters (signal : Signal) : BaseIO Unit := do
  signal.state.atomically do
    let st ← get
    st.notify.state.atomically do
      let notifyState ← get
      let consumers ← notifyState.consumers.filterM fun
        | .normal _ => return true
        | .select waiter => return !(← waiter.checkFinished)
      set { notifyState with consumers }

/--
Check if the signal is currently in signaled state.
-/
def isSignaled (x : Signal) : BaseIO Bool := do
  x.state.atomically do
    let st ← get
    return st.signaled

/--
Wait for the signal, returns immediately if already signaled.
-/
def wait (x : Signal) : BaseIO (AsyncTask Unit) := do
  x.state.atomically do
    let st ← get
    if st.signaled then
      let promise ← IO.Promise.new
      promise.resolve ()
      return promise.result?.map fun
        | some res => .ok res
        | none => .error "dropped"
    else
      st.notify.wait

/--
Creates a selector that waits for the signal to be set.
-/
def selector (signal : Signal) : Selector Unit := {
  tryFn := do
    let signaled ← signal.isSignaled
    if signaled then
      return some ()
    else
      return none

  registerFn := fun waiter => do
    signal.state.atomically do
      let st ← get
      if st.signaled then
        let promise ← IO.Promise.new
        promise.resolve ()
        discard <| waiter.race (return false) (fun p => do p.resolve (.ok ()); return true)
      else
        st.notify.state.atomically do
          modify fun notifyState =>
            { notifyState with consumers := notifyState.consumers.enqueue (.select waiter) }

  unregisterFn := do
    signal.state.atomically do
      let st ← get
      st.notify.state.atomically do
        let notifyState ← get
        let consumers ← notifyState.consumers.filterM fun
          | .normal _ => return true
          | .select waiter => return !(← waiter.checkFinished)
        set { notifyState with consumers }
}

end Signal
end MiniRedis
