/-
Copyright (c) 2025 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import MiniRedis.Util.Frame
import MiniRedis.Util.Cmd.Parse

namespace MiniRedis

structure Ping where
  msg : Option ByteArray

namespace Ping

def ofFrame : CmdParseM Ping := do
  if ← CmdParseM.hasNext then
    let bytes ← CmdParseM.nextBytes
    return Ping.mk bytes
  else
    return Ping.mk none

def toFrame (ping : Ping) : Frame := Id.run do
  let mut frame := Frame.array #[]
  frame := frame.pushBulk "ping".toUTF8
  if let some msg := ping.msg then
    frame := frame.pushBulk msg
  return frame

end Ping

end MiniRedis
