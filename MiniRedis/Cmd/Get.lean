/-
Copyright (c) 2025 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import MiniRedis.Frame
import MiniRedis.Cmd.Basic
import MiniRedis.Connection

/-!
This module implements parsing and interpretation of the Redis `GET` command.
Reference: https://redis.io/docs/latest/commands/get/
-/

namespace MiniRedis

/--
Structure that represents a command for getting the value of a key. Returns a bulk string if
the value of the key exists, otherwise returns NIL.
-/
structure Get where
  key : String

instance : OfFrame Get where
  ofFrame := do
    let key ← CmdParseM.nextString
    return Get.mk key

instance : ToFrame Get where
  toFrame get :=
    Frame.array #[]
    |>.pushBulk "get".toUTF8
    |>.pushBulk get.key.toUTF8

namespace Get

/--
Runs a `Get` with a `Database`.
-/
def handle (get : Get) (db : Database) : ConnectionM Unit := do
  if let some val ← db.get? get.key then
    ConnectionM.writeFrame <| Frame.bulk val
  else
    ConnectionM.writeFrame Frame.null

end Get
end MiniRedis
