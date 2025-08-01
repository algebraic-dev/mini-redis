/-
Copyright (c) 2025 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import MiniRedis.Frame
import MiniRedis.Util.Signal
import Std.Internal.Async.TCP

/-!
Provides the `ConnectionM` monad that can be used to communicate with a partner that understands
the Redis protocol by sending and receiving `Frame`s.
-/

namespace MiniRedis
open Std.Internal.IO.Async

private structure Connection where
  -- TODO: This should use a buffered writer instead.
  client : TCP.Socket.Client
  buf : ByteArray
  idx : Nat
  shutdown : Signal

abbrev ConnectionM := StateRefT Connection Async

instance : MonadLift IO ConnectionM where
  monadLift m := m

namespace ConnectionM

def run (x : ConnectionM α) (client : TCP.Socket.Client) : Async α := do
  StateRefT'.run' x { client, buf := .empty, idx := 0 , shutdown := (← Signal.new) }

/--
Attempt to parse a frame from the internal buffer. If the buffer does contain enough data the frame
gets read, the buffer shrinked accordingly and `some frame` is returned. If not enough data is
available yet `none` is returned.
-/
private def parseFrame : ConnectionM (Option Frame) := do
  let conn ← get
  let iter := ByteArray.Iterator.mk conn.buf conn.idx
  match Frame.check iter with
  | .ok _ =>
    match Frame.parseChecked iter with
    | .ok (frame, ⟨buf, endIdx⟩) =>
      -- TODO: This shows we need something like Tokio Bytes/BytesMut for optimal perf
      modify fun conn => { conn with buf := buf.extract endIdx buf.size , idx := 0 }
      return frame
    | .error (.other e) => throw <| .userError e
    | .error .incomplete => unreachable!
  | .error .incomplete => return none
  | .error (.other e) => throw <| .userError e

/--
Read a single `Frame` from the TCP connection.

This function will (asynchronously) wait until enough data is available to actually parse a `Frame`
from it and return it as `some frame`. If the TCP connection is closed in a way that doesn't break a
frame in half it will return `none` instead, otherwise throw an error.
-/
partial def readFrame (cancel : Signal) : ConnectionM (Option Frame) := do
  while true do
    if let some frame ← parseFrame then
      return some frame

    let recv ← (← get).client.recvSelector 4096

    let buf? ← await (← Selectable.one #[
      .case recv (fun x => pure <| pure (some x)),
      .case cancel.selector (fun _ => pure <| pure none)
    ])

    match buf? with
    | some (some buf) =>
      modify fun conn => { conn with buf := conn.buf ++ buf }
    | some none =>
      if (← get).buf.size == (← get).idx then
        return none
      else
        throw <| .userError "Connection reset by peer"
    | none =>
      return none

  return none

@[inline]
private def writeU8 (byte : UInt8) : ConnectionM Unit := do
  await <| ← (← get).client.send (ByteArray.mk #[byte])

@[inline]
private def writeBytes (buf : ByteArray) : ConnectionM Unit := do
  await <| ← (← get).client.send buf

@[inline]
private def writeDecimal (dec : Int64) : ConnectionM Unit := do
  writeBytes s!"{dec}\r\n".toUTF8

private def writeValue (f : Frame) : ConnectionM Unit := do
  match f with
  | .simple val =>
    writeU8 '+'.toUInt8
    writeBytes val.toUTF8
    writeBytes "\r\n".toUTF8
  | .error val =>
    writeU8 '-'.toUInt8
    writeBytes val.toUTF8
    writeBytes "\r\n".toUTF8
  | .integer val =>
    writeU8 ':'.toUInt8
    writeDecimal val
  | .null => writeBytes "$-1\r\n".toUTF8
  | .bulk val =>
    let len := val.size

    writeU8 '$'.toUInt8
    writeDecimal len.toInt64
    writeBytes val
    writeBytes "\r\n".toUTF8
  | .array .. => unreachable!

/--
Write a `Frame` to the underlying TCP connection.
-/
def writeFrame (f : Frame) : ConnectionM Unit := do
  /-
  Arrays are handled specially as recursively encoding arrays is not trivially possible so we only
  support encoding arrays as the top level structure for now.
  -/
  match f with
  | .array val =>
    writeU8 '*'.toUInt8
    writeDecimal val.size.toInt64
    for entry in val do
      writeValue entry
  | _ => writeValue f

end ConnectionM

end MiniRedis
