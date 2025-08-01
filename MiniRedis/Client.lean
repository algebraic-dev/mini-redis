/-
Copyright (c) 2025 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import MiniRedis.Connection
import MiniRedis.Cmd

namespace MiniRedis

open Std.Internal.IO.Async

abbrev ClientM := ConnectionM

namespace ClientM

def run (x : ClientM α) (addr : Std.Net.SocketAddress) : Async α := do
  let client ← TCP.Socket.Client.mk
  await <| (← client.connect addr)
  ConnectionM.run x client

private def readResponse : ClientM Frame := do
  let response? ← ConnectionM.readFrame (← Signal.new)
  match response? with
  | some (.error msg) =>
    throw <| .userError s!"Received error from server: {msg}"
  | some frame =>
    return frame
  | none =>
    throw <| .resourceVanished 0 "connection reset by server"

def ping (msg : Option ByteArray) : ClientM ByteArray := do
  let frame := ToFrame.toFrame <| Ping.mk msg
  ConnectionM.writeFrame frame
  match ← readResponse with
  | .simple value => return value.toUTF8
  | .bulk value => return value
  | frame => throw <| .userError s!"Invalid ping response: {repr frame}"

def get (key : String) : ClientM (Option ByteArray) := do
  let frame := ToFrame.toFrame <| Get.mk key
  ConnectionM.writeFrame frame
  match ← readResponse with
  | .simple value => return some value.toUTF8
  | .bulk value => return some value
  | .null => return none
  | frame => throw <| .userError s!"Invalid get response: {repr frame}"

def set (key : String) (value : ByteArray) (expiration : Option Std.Time.Duration := none) : ClientM Unit := do
  let frame := ToFrame.toFrame <| Set.mk key value expiration
  ConnectionM.writeFrame frame

  match ← readResponse with
  | .simple msg =>
    if msg == "OK" then
      return ()
    else
      throw <| .userError s!"Invalid set response: {msg}"
  | frame => throw <| .userError s!"Invalid set response: {repr frame}"

end ClientM
end MiniRedis
