/-
Copyright (c) 2025 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import MiniRedis.Util.Frame
import MiniRedis.Util.Cmd.Parse
import MiniRedis.Util.Connection
import MiniRedis.Db

namespace MiniRedis

structure Publish where
  channel : String
  message : ByteArray

namespace Publish

def ofFrame : CmdParseM Publish := do
  let channel ← CmdParseM.nextString
  let messages ← CmdParseM.nextBytes
  return Publish.mk channel messages

def handle (pub : Publish) (db : Database) : ConnectionM Unit := do
  let numSubscribers ← db.publish pub.channel pub.message
  let frame := Frame.array #[] |>.pushInt numSubscribers
  ConnectionM.writeFrame <| frame

def toFrame (pub : Publish) : Frame := Id.run do
  let mut frame := Frame.array #[]
  frame := frame.pushBulk "publish".toUTF8
  frame := frame.pushBulk pub.channel.toUTF8
  frame := frame.pushBulk pub.message
  return frame

end Publish

end MiniRedis
