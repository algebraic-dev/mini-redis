/-
Copyright (c) 2025 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import MiniRedis.Util.Parser

/-!
This module provides a type that represents Redis frames as well as tools to parse them.
-/

namespace MiniRedis

/--
Represents the different types of frames available in the supported
[`RESP2`](https://redis.io/docs/latest/develop/reference/protocol-spec/#resp-protocol-description)
fragment.
-/
inductive Frame where
  | simple (msg : String)
  | error (msg : String)
  | integer (int : Int64)
  | bulk (bytes : ByteArray)
  | null
  | array (arr : Array Frame)
deriving Inhabited, BEq, Repr

namespace Frame

/--
Check whether `buf` currently contains enough data to parse at least one `Frame` from it.
If not enough data is available will return `.error .incomplete`.
-/
partial def check (buf : ByteArray.Iterator) : Except Error Unit :=
  ParseM.run' go buf
where
  go : ParseM Unit := do
    let head ← ParseM.getU8
    -- TODO: byte literals b'+'
    -- TODO: support UInt8 matches like match head with | b'+' =>
    if head == '+'.toUInt8 then
      discard <| ParseM.getLine
    else if head == '-'.toUInt8 then
      discard <| ParseM.getLine
    else if head == ':'.toUInt8 then
      discard <| ParseM.getDecimal
    else if head == '$'.toUInt8 then
      if (← ParseM.peekU8) == '-'.toUInt8 then
        -- Skip '-1\r\n'
        ParseM.skip 4
      else
        -- Read bulk string
        let len ← ParseM.getDecimal
        -- skip that number of bytes + 2 (\r\n)
        ParseM.skip (len.toNatClampNeg + 2)
    else if head == '*'.toUInt8 then
      let len ← ParseM.getDecimal
      if len < -1 then
        ParseM.throwInvalid
      for _ in [0:len.toNatClampNeg] do
        go
    else
      throw <| .other s!"protocol error; invalid frame type byte `{head}`"

/--
Parses simple strings in the format +OK\r\n

* https://redis.io/docs/latest/develop/reference/protocol-spec/#simple-strings
-/
def parseSimpleString : ParseM Frame := do
  let line ← ParseM.getLine
  let some string := String.fromUTF8? line
    | throw <| .other s!"Failed to convert buffer to string: {line}"
  return .simple string

/--
Parses simple errors in the format -Error message\r\n

* https://redis.io/docs/latest/develop/reference/protocol-spec/#simple-errors
-/
def parseSimpleError : ParseM Frame := do
  let line ← ParseM.getLine
  let some string := String.fromUTF8? line
    | throw <| .other s!"Failed to convert buffer to string: {line}"
  return .error string

/--
Parses integers in the format :[<+|->]<value>\r\n

* https://redis.io/docs/latest/develop/reference/protocol-spec/#integers
-/
def parseInteger : ParseM Frame := do
  let len ← ParseM.getDecimal
  return .integer len

/--
Parses bulk strings in the format $<length>\r\n<data>\r\n

* https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings
* https://redis.io/docs/latest/develop/reference/protocol-spec/#null-bulk-strings
-/
def parseBulkString : ParseM Frame := do
  if (← ParseM.peekU8) == '-'.toUInt8 then
    let line ← ParseM.getLine
    if line != "-1".toUTF8 then
      ParseM.throwInvalid
    return .null
  else
    let len ← ParseM.getDecimal
    let data ← ParseM.getBytes len.toNatClampNeg
    ParseM.skip 2
    return .bulk data

mutual

/--
Parses arrays in the format *<count>\r\n<element1><element2>...<elementN>

* https://redis.io/docs/latest/develop/reference/protocol-spec/#arrays
* https://redis.io/docs/latest/develop/reference/protocol-spec/#null-arrays
-/
partial def parseArray : ParseM Frame := do
  if (← ParseM.peekU8) == '-'.toUInt8 then
    let line ← ParseM.getLine
    if line != "-1".toUTF8 then
      ParseM.throwInvalid
    return .null
  else
    let len ← ParseM.getDecimal
    let mut out := Array.emptyWithCapacity len.toNatClampNeg
    for _ in [0:len.toNatClampNeg] do
      out := out.push (← parseFrame)
    return .array out

/--
Parses one `Frame`.

* https://redis.io/docs/latest/develop/reference/protocol-spec
-/
partial def parseFrame : ParseM Frame := do
  let head ← ParseM.getU8

  if head == '+'.toUInt8 then
    parseSimpleString
  else if head == '-'.toUInt8 then
    parseSimpleError
  else if head == ':'.toUInt8 then
    parseInteger
  else if head == '$'.toUInt8 then
    parseBulkString
  else if head == '*'.toUInt8 then
    parseArray
  else
    unreachable!

end

/--
Parse one `Frame` from `buf`, assuming that `check` has already been called on it to ensure we have
data available.
-/
def parseChecked (buf : ByteArray.Iterator) : Except Error (Frame × ByteArray.Iterator) :=
  ParseM.run parseFrame buf

/--
Run `check`, on success `parseChecked` on `buf`.
-/
def parse (buf : ByteArray.Iterator) : Except Error (Frame × ByteArray.Iterator) := do
  discard <| check buf
  parseChecked buf

/--
Pushes a `ByteArray` into the a `Frame.array`.
-/
def pushBulk (f : Frame) (b : ByteArray) : Frame :=
  match f with
  | .array vec => .array <| vec.push <| .bulk b
  | _ => panic! "not an array frame"

/--
Pushes an `Int` into the a `Frame.array`.
-/
def pushInt (f : Frame) (int : Int64) : Frame :=
  match f with
  | .array vec => .array <| vec.push <| .integer int
  | _ => panic! "not an array frame"

end Frame
end MiniRedis
