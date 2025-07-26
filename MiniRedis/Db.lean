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

structure Database.Entry where
  value : ByteArray
  expiresAt : Option Std.Time.Timestamp

structure Database.State where
  -- Use a `TreeMap` instead of `HashMap` because linearity in `Mutex` doesn't work yet
  map : Std.TreeMap String Database.Entry
  expirations : Std.TreeSet (Std.Time.Timestamp × String)
  pubSub : Std.HashMap String (Broadcast ByteArray)

structure Database where
  state : Std.Mutex Database.State
  backgroundTaskChannel : Std.Channel Unit

namespace Database

namespace State

def nextExpiration (s : State) : Option Std.Time.Timestamp :=
  s.expirations.min?.map Prod.fst

end State

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

def new : Async Database := do
  let state ← Std.Mutex.new { map := {}, expirations := {}, pubSub := {} }
  let backgroundTaskChannel ← Std.Channel.new (some 0)
  let db := { state, backgroundTaskChannel }
  discard <| async (purgeExpirations db)
  return db

def get? (db : Database) (key : String) : BaseIO (Option ByteArray) :=
  db.state.atomically do
    return (← get).map.get? key |>.map Entry.value

def set (db : Database) (key : String) (value : ByteArray) (expire : Option Std.Time.Duration) :
    IO Unit := do
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

def publish (db : Database) (key : String) (value : ByteArray) : IO Nat := do
  db.state.atomically do
    -- TODO: correct error handling
    let foo ← (← get).pubSub[key]?.mapM (fun b => b.send value)
    match foo with
    | some _ => return 10
    | none => return 0

end Database


end MiniRedis
