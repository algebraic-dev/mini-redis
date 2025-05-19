/-
Copyright (c) 2025 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import Std.Time.Duration
import Std.Sync.Mutex
import Std.Data.HashMap

namespace MiniRedis

structure Database.Entry where
  value : ByteArray
  -- TODO: expiresAt

structure Database.State where
  map : Std.HashMap String Database.Entry

structure Database where
  state : Std.Mutex Database.State

namespace Database

def new : BaseIO Database := do
  let state ← Std.Mutex.new { map := {} }
  return { state }

def get? (db : Database) (key : String) : BaseIO (Option ByteArray) :=
  db.state.atomically do
    return (← get).map.get? key |>.map Entry.value

-- TODO: expire
def set (db : Database) (key : String) (value : ByteArray) (_expire : Option Std.Time.Duration) :
    BaseIO Unit := do
  db.state.atomically do
    modify fun s => { s with map := s.map.insert key { value }}

end Database


end MiniRedis
