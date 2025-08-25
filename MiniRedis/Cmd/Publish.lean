/-
Copyright (c) 2025 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import MiniRedis.Frame
import MiniRedis.Cmd.Basic
import MiniRedis.Connection

/-!
This module implements parsing and interpretation of the Redis `PUBLISH` command.
Reference: https://redis.io/docs/latest/commands/publish
-/

namespace MiniRedis

/--
Structure that represents a command for posting a message to a given channel.
-/
structure Publish where
  channel : String
  message : ByteArray

instance : OfFrame Publish where
  ofFrame := do
    let channel ← CmdParseM.nextString
    let messages ← CmdParseM.nextBytes
    return Publish.mk channel messages

instance : ToFrame Publish where
  toFrame pub := Id.run do
    let mut frame := Frame.array #[]
    frame := frame.pushBulk "publish".toUTF8
    frame := frame.pushBulk pub.channel.toUTF8
    frame := frame.pushBulk pub.message
    return frame

namespace Publish

/--
Runs a `Publish` with a `Database`.
-/
def handle (pub : Publish) (db : Database) : ConnectionM Unit := do
  let numSubscribers ← db.publish pub.channel pub.message
  let frame := Frame.array #[] |>.pushInt (numSubscribers.getD 0).toInt64
  ConnectionM.writeFrame frame

end Publish
end MiniRedis
