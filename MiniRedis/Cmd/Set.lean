/-
Copyright (c) 2025 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import MiniRedis.Frame
import MiniRedis.Cmd.Basic
import MiniRedis.Connection

/-!
This module implements parsing and interpretation of the Redis `SET` command.
Reference: https://redis.io/docs/latest/commands/set/
-/

namespace MiniRedis
open Std.Time

/--
Structure representing a command to set a key's value. Supports optional parameters like TTL for
expiration.
-/
structure Set where
  key : String
  value : ByteArray
  expire : Option Duration

instance : OfFrame Set where
  ofFrame := do
    let key ← CmdParseM.nextString
    let value ← CmdParseM.nextBytes

    if ← CmdParseM.hasNext then
      let timeoutType := (← CmdParseM.nextString).toUpper
      match timeoutType with
      | "EX" =>
        let int ← CmdParseM.nextInt
        let duration := Duration.ofSeconds <| .ofInt int.toInt
        return Set.mk key value (some duration)
      | "PX" =>
        let int ← CmdParseM.nextInt
        let duration := Duration.ofMillisecond <| .ofInt int.toInt
        return Set.mk key value (some duration)
      | _ => throw <| .other "Currently SET only supports the expiration option"
    else
      return Set.mk key value none

instance : ToFrame Set where
  toFrame set := Id.run do
    let mut frame := Frame.array #[]
    frame := frame.pushBulk "set".toUTF8
    frame := frame.pushBulk set.key.toUTF8
    frame := frame.pushBulk set.value
    if let some duration := set.expire then
      frame := frame.pushBulk "px".toUTF8
      frame := frame.pushInt duration.toMilliseconds.toInt.toInt64
    return frame

namespace Set

/--
Runs a `Set` with a `Database`.
-/
def handle (set : Set) (db : Database) : ConnectionM Unit := do
  db.set set.key set.value set.expire
  ConnectionM.writeFrame <| .simple "OK"

end Set
end MiniRedis
