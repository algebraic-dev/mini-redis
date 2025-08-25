/-
Copyright (c) 2025 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Sofia Rodrigues
-/
import Std.Internal.Async.Select
import MiniRedis.Util.Signal
import Std.Data.TreeMap
import Std

open Std.Internal.IO.Async

namespace MiniRedis

def isCancelled (x : IO.Promise Bool) : IO Bool := do
  if ¬ (← x.isResolved) then return false else return x.result!.get

def cancellableSelector [Monad m] [MonadLift IO m] [MonadAsync AsyncTask m] (fn : IO.Promise Bool → m α) : m (Selector (Except IO.Error α)) := do
  let signal ← IO.Promise.new
  let promise ← IO.Promise.new
  let result : AsyncTask α ← async (fn signal)

  IO.chainTask result (promise.resolve ·)

  return {
    tryFn := do
      if ← promise.isResolved
        then return promise.result!.get
        else return none

    registerFn := fun waiter => do
      discard <| IO.mapTask (t := promise.result?) fun
        | none => pure ()
        | some res => do
          if ¬ (← isCancelled signal) then
            waiter.race (pure ()) (·.resolve (.ok res))

    unregisterFn := do
      signal.resolve true
  }


end MiniRedis
