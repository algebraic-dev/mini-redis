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

/--
Represents a limit on concurrent connections using a channel as a semaphore.
-/
def ConnectionLimit := Std.Channel Unit

namespace ConnectionLimit

/--
Initializes a new `ConnectionLimit` with `maxConnections` available slots.
-/
def new (maxConnections : Nat) : Async ConnectionLimit := do
  let connectionLimit ← Std.Channel.new (some maxConnections)

  for _ in [:maxConnections] do
    connectionLimit.sync.send ()

  return connectionLimit

/--
Acquires a connection slot by waiting for an available token.
-/
def adquire (limit : ConnectionLimit) : Async Unit := do
  await (← limit.recv)

/--
Releases a connection slot by returning a token to the channel.
-/
def release (limit : ConnectionLimit) : Async Unit :=
  discard <| limit.send ()

end ConnectionLimit
end MiniRedis
