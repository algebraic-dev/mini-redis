/-
Copyright (c) 2025 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Sofia Rodrigues
-/
import MiniRedis.Frame
import MiniRedis.Connection
import MiniRedis.Db

namespace MiniRedis

/--
Tracks parsing state with frame array and current index position
-/
structure ParseState where
  arr : Array Frame
  idx : Nat

/--
Represents command parsing errors that can occur during frame processing
-/
inductive CmdParseError where
  | endOfStream
  | other (e : String)

/--
Monadic parser combining state tracking with error handling for command parsing
-/
abbrev CmdParseM := StateT ParseState <| Except CmdParseError

namespace CmdParseM

/--
Runs a parser computation on a frame, initializing the parse state
-/
def run (x : CmdParseM α) (f : Frame) : Except CmdParseError α :=
  let arr :=
    match f with
    | .array arr => arr
    | f => #[f]
  StateT.run' x { arr, idx := 0 }

private def next : CmdParseM Frame := do
  let s ← get
  if h : s.idx < s.arr.size then
    let ret := s.arr[s.idx]'h
    modify fun s => { s with idx := s.idx + 1 }
    return ret
  else
    throw .endOfStream

/--
Parses the next frame as a UTF-8 string from simple or bulk frames
-/
def nextString : CmdParseM String := do
  match ← next with
  | .simple s => return s
  | .bulk data =>
    let some str := String.fromUTF8? data
      | throw <| .other "protocol error; invalid string"
    return str
  | _ => throw <| .other "protocol error; expected string frame but got something else"

/--
Parses the next frame as raw bytes from simple or bulk frames
-/
def nextBytes : CmdParseM ByteArray := do
  match ← next with
  | .simple s => return s.toUTF8
  | .bulk data => return data
  | _ => throw <| .other "protocol error; expected string frame but got something else"

/--
Parses the next frame as a 64-bit integer from various frame types
-/
def nextInt : CmdParseM Int64 := do
  match ← next with
  | .integer int => return int
  | .simple s =>
    let some int := String.toInt? s
      | throw <| .other "protocol error; invalid int"
    return int.toInt64
  | .bulk data =>
    -- TODO: Better int parsing
    let some s := String.fromUTF8? data
      | throw <| .other "protocol error; invalid int"

    let some int := String.toInt? s
      | throw <| .other "protocol error; invalid int"

    return int.toInt64
  | _ => throw <| .other "protocol error; expected string frame but got something else"

/--
Verifies that all frames have been consumed during parsing
-/
def finish : CmdParseM Unit := do
  let s ← get
  if s.arr.size == s.idx then
    return ()
  else
    throw <| .other "protocol error; expected end of frame, but there was more"

/--
Checks if there are more frames available for parsing
-/
def hasNext : CmdParseM Bool := do
  let s ← get
  return s.idx < s.arr.size

end CmdParseM
end MiniRedis
