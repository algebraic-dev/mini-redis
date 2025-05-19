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

structure Set where
  key : String
  value : ByteArray
  expire : Option Std.Time.Duration

namespace Set

def ofFrame : CmdParseM Set := do
  let key ← CmdParseM.nextString
  let value ← CmdParseM.nextBytes

  if ← CmdParseM.hasNext then
    let timeoutType := (← CmdParseM.nextString).toUpper
    match timeoutType with
    | "EX" =>
      let int ← CmdParseM.nextInt
      let duration := Std.Time.Duration.ofSeconds <| .ofInt int.toInt
      return Set.mk key value (some duration)
    | "PX" =>
      let int ← CmdParseM.nextInt
      let duration := Std.Time.Duration.ofMillisecond <| .ofInt int.toInt
      return Set.mk key value (some duration)
    | _ => throw <| .other "Currently SET only supports the expiration option"
  else
    return Set.mk key value none

def handle (set : Set) (db : Database) : ConnectionM Unit := do
  db.set set.key set.value set.expire
  ConnectionM.writeFrame <| .simple "OK"

def toFrame (set : Set) : Frame := Id.run do
  let mut frame := Frame.array #[]
  frame := frame.pushBulk "set".toUTF8
  frame := frame.pushBulk set.key.toUTF8
  frame := frame.pushBulk set.value
  if let some duration := set.expire then
    frame := frame.pushBulk "px".toUTF8
    frame := frame.pushInt duration.toMilliseconds.toInt.toInt64
  return frame

end Set

end MiniRedis
