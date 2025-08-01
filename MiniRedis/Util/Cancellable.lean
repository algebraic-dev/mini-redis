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

def cancellableSelector [Monad m] [MonadLift IO m] [MonadAsync AsyncTask m] (fn : Signal → m α) : m (Selector (Except IO.Error α)) := do
  let signal ← Signal.new
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
        | some res => waiter.race (pure ()) (·.resolve (.ok res))

    unregisterFn := signal.signal
  }


end MiniRedis
