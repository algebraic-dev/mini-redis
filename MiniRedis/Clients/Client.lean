/-
Copyright (c) 2025 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import MiniRedis.Util.Connection
import MiniRedis.Util.Cmd

namespace MiniRedis

open Std.Internal.IO.Async

abbrev ClientM := ConnectionM

namespace ClientM

def run (x : ClientM α) (addr : Std.Net.SocketAddress) : Async α := do
  let client ← TCP.Socket.Client.mk
  await <| (← client.connect addr)
  ConnectionM.run x client

private def readResponse : ClientM Frame := do
  let response? ← ConnectionM.readFrame
  match response? with
  | some (.error msg) => throw <| .userError s!"Received error from server: {msg}"
  | some frame => return frame
  | none =>
    throw <| .resourceVanished 0 "connection reset by server"

def ping (msg : Option ByteArray) : ClientM ByteArray := do
  let frame := Ping.mk msg |>.toFrame
  ConnectionM.writeFrame frame
  match ← readResponse with
  | .simple value => return value.toUTF8
  | .bulk value => return value
  | _ => throw <| .userError s!"Invalid ping response: {repr frame}"

end ClientM

end MiniRedis
