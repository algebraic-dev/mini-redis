/-
Copyright (c) 2025 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import MiniRedis.Util.Frame
import MiniRedis.Util.Cmd.Parse

namespace MiniRedis

structure Get where
  key : String

namespace Get

def ofFrame : CmdParseM Get := do
  let key ← CmdParseM.nextString
  return Get.mk key

def toFrame (get : Get) : Frame := Id.run do
  let mut frame := Frame.array #[]
  frame := frame.pushBulk "get".toUTF8
  frame := frame.pushBulk get.key.toUTF8
  return frame

end Get

end MiniRedis
