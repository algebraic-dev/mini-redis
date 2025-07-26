/-
Copyright (c) 2025 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import MiniRedis.Frame
import MiniRedis.Cmd.Basic
import MiniRedis.Connection
import MiniRedis.Db

namespace MiniRedis

structure Get where
  key : String

namespace Get

/--
Parses a `String` into a `Get`.
-/
def ofFrame : CmdParseM Get := do
  let key ← CmdParseM.nextString
  return Get.mk key

/--
Runs a `Get` with a `Database`.
-/
def handle (get : Get) (db : Database) : ConnectionM Unit := do
  if let some val ← db.get? get.key then
    ConnectionM.writeFrame <| Frame.bulk val
  else
    ConnectionM.writeFrame Frame.null

/--
Creates a `Frame` out of a `Get`.
-/
def toFrame (get : Get) : Frame := Id.run do
  let mut frame := Frame.array #[]
  frame := frame.pushBulk "get".toUTF8
  frame := frame.pushBulk get.key.toUTF8
  return frame

end Get
end MiniRedis
