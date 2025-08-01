/-
Copyright (c) 2025 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import MiniRedis.Frame
import MiniRedis.Cmd.Basic
import MiniRedis.Connection

/-!
This module implements parsing and interpretation of the Redis `PING` command.
Reference: https://redis.io/docs/latest/commands/ping
-/

namespace MiniRedis

/--
Structure that represents a command for checking the connection status, availability and latency. Returns
PONG if no argument is provided, otherwise return a copy of the argument as a bulk.
-/
structure Ping where
  msg : Option ByteArray

instance : OfFrame Ping where
  ofFrame := do
    if ← CmdParseM.hasNext then
      let bytes ← CmdParseM.nextBytes
      return Ping.mk bytes
    else
      return Ping.mk none

instance : ToFrame Ping where
  toFrame ping := Id.run do
    let mut frame := Frame.array #[]
    frame := frame.pushBulk "ping".toUTF8
    if let some msg := ping.msg then
      frame := frame.pushBulk msg
    return frame

namespace Ping

/--
Runs a `Ping` with a `Database`.
-/
def handle (p : Ping) : ConnectionM Unit := do
  let response :=
    match p.msg with
    | none => Frame.simple "PONG"
    | some msg => Frame.bulk msg
  ConnectionM.writeFrame response

end Ping
end MiniRedis
