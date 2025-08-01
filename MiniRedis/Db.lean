/-
Copyright (c) 2025 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import Std.Time.Duration
import Std.Time.DateTime.Timestamp
import Std.Sync.Mutex
import Std.Sync.Channel
import Std.Data.TreeMap
import Std.Data.TreeSet
import Std.Internal.Async.Basic
import Std.Internal.Async.Timer
import MiniRedis.Util.Broadcast

namespace MiniRedis

open Std.Internal.IO.Async
open Std.Time

/--
An entry in the database containing a value and optional expiration timestamp
-/
structure Database.Entry where
  value : ByteArray
  expiresAt : Option Std.Time.Timestamp

/--
The internal state of the database containing the key-value store, expirations, and pub-sub channels
-/
structure Database.State where
  -- Use a `TreeMap` instead of `HashMap` because linearity in `Mutex` doesn't work yet
  map : Std.TreeMap String Database.Entry

  /--
  Tracks key time to live timestamps.
  -/
  expirations : Std.TreeSet (Std.Time.Timestamp × String)

  /--
  Pub/Sub System.
  -/
  pubSub : Std.HashMap String (Broadcast ByteArray)

/--
Get the timestamp of the next key expiration, if any
-/
def Database.State.nextExpiration (s : State) : Option Std.Time.Timestamp :=
  s.expirations.min?.map Prod.fst

/--
A thread-safe Redis-like database with expiration and pub-sub support
-/
structure Database where
  state : Std.Mutex Database.State
  backgroundTaskChannel : Std.Channel Unit

namespace Database

private def purgeExpiredKeys (db : Database) : IO (Option Std.Time.Timestamp) :=
  db.state.atomically do
    let now ← Std.Time.Timestamp.now
    while true do
      if let some (when, key) := (← get).expirations.min? then
        -- Our timers right now only have millisecond precision so let's account
        if 0 < (when - now).toMilliseconds then
          return some when
        else
          modify fun s =>
            {
              s with
                map := s.map.erase key,
                expirations := s.expirations.erase (when, key)
            }
      else
        break

    return none

private partial def purgeExpirations (db : Database) : Async Unit := do
  while true do
    if let some when ← purgeExpiredKeys db then
      let now ← Std.Time.Timestamp.now
      let dur := when - now
      IO.println s!"Purge: Will wait for {dur.toMilliseconds}ms"
      await <| ← Selectable.one #[
        .case db.backgroundTaskChannel.recvSelector (fun _ => return AsyncTask.pure ()),
        .case (← Selector.sleep dur.toMilliseconds) (fun _ => return AsyncTask.pure ())
      ]
      IO.println s!"Purge: Awoken"
    else
      await <| ← db.backgroundTaskChannel.recv

/--
Create a new database instance with background expiration task
-/
def new : Async Database := do
  let state ← Std.Mutex.new { map := {}, expirations := {}, pubSub := {} }
  let backgroundTaskChannel ← Std.Channel.new (some 0)
  let db := { state, backgroundTaskChannel }
  discard <| async (purgeExpirations db)
  return db

/--
Get the value associated with a key, returning None if key doesn't exist or has expired
-/
def get? (db : Database) (key : String) : BaseIO (Option ByteArray) :=
  db.state.atomically do
    return (← get).map.get? key |>.map Entry.value

/--
Set a key-value pair with optional expiration duration
-/
def set (db : Database) (key : String) (value : ByteArray) (expire : Option Std.Time.Duration) : IO Unit := do
  let notify ← db.state.atomically do
    let (expiresAt, notify) ←
      if let some dur := expire then
        let now ← Std.Time.Timestamp.now
        let when := now + dur
        -- TODO: https://github.com/leanprover/lean4/issues/8431
        let notify := (← get).nextExpiration |>.map (· > when : _ → Bool) |>.getD true
        pure (some when, notify)
      else
        pure (none, false)

    let prev? ← modifyGet fun s =>
      let prev? := s.map[key]?
      (prev?, { s with map := s.map.insert key { value, expiresAt } })

    if let some prev := prev? then
      if let some when := prev.expiresAt then
        modify fun s => { s with expirations := s.expirations.erase (when, key) }

    if let some when := expiresAt then
      modify fun s => { s with expirations := s.expirations.insert (when, key) }

    return notify

  if notify then
    -- the background task should always be ready so no need for async stuff
    db.backgroundTaskChannel.sync.send ()

/--
Subscribes to a new channel returning a new receiver
-/
def subscribe (db : Database) (key : String) : IO (Broadcast.Receiver ByteArray) := do
  db.state.atomically do
    let env ← get

    let (receiver, broadcast) ←
      match env.pubSub[key]? with
      | some broadcast => broadcast.receiver
      | none => Broadcast.new 1024 |>.receiver

    MonadState.set { env with pubSub := env.pubSub.insert key broadcast }
    pure receiver

/--
Publish a message to all subscribers of a channel, returning the number of subscribers
-/
def publish (db : Database) (key : String) (value : ByteArray) : IO Nat := do
  db.state.atomically do
    -- TODO: correct error handling
    match ← (← get).pubSub[key]?.mapM (fun b => b.send value) with
    | some res => return res
    | none => return 0

end Database
end MiniRedis
