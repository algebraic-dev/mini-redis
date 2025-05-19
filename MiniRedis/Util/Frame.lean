/-
Copyright (c) 2025 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/

/-!
This module provides a type that represents Redis frames as well as tools to parse them.
-/

namespace MiniRedis

-- TODO: This should be upstreamed and use memcmp
instance : BEq ByteArray where
  beq lhs rhs := Id.run do
    if lhs.size = rhs.size then
      for i in [0:lhs.size] do
        if lhs[i]! != rhs[i]! then
          return false
      return true
    else
      false

instance : Repr ByteArray where
  reprPrec arr n := reprPrec arr.data n

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
Errors that might happen while parsing a frame
-/
inductive Error where
  /--
  We do not yet have enough data to consume a frame from the input buffer.
  -/
  | incomplete
  /--
  An actual error described by a `String`.
  -/
  | other (err : String)
  deriving Inhabited, BEq, Repr

private abbrev ParseM := ExceptT Error <| StateM (ByteArray.Iterator)

namespace ParseM

@[inline]
private def throwInvalid : ParseM α :=
  throw <| .other "protocol error; invalid frame format"

private def run' (x : ParseM α) (buf : ByteArray.Iterator) : Except Error α :=
  let res := StateT.run' (ExceptT.run x) buf
  res

private def run (x : ParseM α) (buf : ByteArray.Iterator) :
    Except Error (α × ByteArray.Iterator) :=
  let (res, it) := StateT.run (ExceptT.run x) buf
  match res with
  | .ok v => .ok (v, it)
  | .error e => .error e

@[inline]
private def getU8 : ParseM UInt8 := do
  if (← get).hasNext then
    modifyGet fun it => (it.curr, it.next)
  else
    throw .incomplete

@[inline]
private def peekU8 : ParseM UInt8 := do
  let it ← get
  if it.hasNext then
    return it.curr
  else
    throw .incomplete

@[inline]
private def skip (count : Nat) : ParseM Unit := do
  if (← get).remainingBytes < count then
    throw .incomplete
  else
    modify fun it => it.forward count

@[inline]
private partial def getLine : ParseM ByteArray := do
  go ByteArray.empty
where
  go (buf : ByteArray) : ParseM ByteArray := do
    let byte ← getU8
    if byte == '\r'.toUInt8 && (← peekU8) == '\n'.toUInt8 then
      discard <| getU8
      return buf
    else
      let buf := buf.push byte
      go buf

@[inline]
private def getDecimal : ParseM Int64 := do
  let line ← getLine
  let some string := String.fromUTF8? line | throwInvalid
  let some int := String.toInt? string | throwInvalid
  if int < Int64.minValue.toInt || int > Int64.maxValue.toInt then
    throwInvalid
  else
    return int.toInt64

@[inline]
private def getBytes (count : Nat) : ParseM ByteArray := do
  if (← get).remainingBytes < count then
    throw .incomplete
  else
    modifyGet fun it => (it.array.extract it.pos (it.pos + count), it.forward count)

end ParseM

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
      if len < 0 then
        ParseM.throwInvalid

      for _ in [0:len.toNatClampNeg] do
        go
    else
      throw <| .other s!"protocol error; invalid frame type byte `{head}`"

/--
Parse one `Frame` from `buf`, assuming that `check` has already been called on it to ensure we have
data available.
-/
partial def parseChecked (buf : ByteArray.Iterator) :
    Except Error (Frame × ByteArray.Iterator) :=
  ParseM.run go buf
where
  go : ParseM Frame := do
    let head ← ParseM.getU8
    if head == '+'.toUInt8 then
      let line ← ParseM.getLine
      let some string := String.fromUTF8? line
        | throw <| .other s!"Failed to convert buffer to string: {line}"
      return .simple string
    else if head == '-'.toUInt8 then
      let line ← ParseM.getLine
      let some string := String.fromUTF8? line
        | throw <| .other s!"Failed to convert buffer to string: {line}"
      return .error string
    else if head == ':'.toUInt8 then
      let len ← ParseM.getDecimal
      return .integer len
    else if head == '$'.toUInt8 then
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
    else if head == '*'.toUInt8 then
      let len ← ParseM.getDecimal
      let mut out := Array.emptyWithCapacity len.toNatClampNeg
      for _ in [0:len.toNatClampNeg] do
        out := out.push (← go)
      return .array out
    else
      unreachable!

/--
Run `check`, on success `parseChecked` on `buf`.
-/
def parse (buf : ByteArray.Iterator) : Except Error (Frame × ByteArray.Iterator) := do
  discard <| check buf
  parseChecked buf

def pushBulk (f : Frame) (b : ByteArray) : Frame :=
  match f with
  | .array vec => .array <| vec.push <| .bulk b
  | _ => panic! "not an array frame"

def pushInt (f : Frame) (int : Int64) : Frame :=
  match f with
  | .array vec => .array <| vec.push <| .integer int
  | _ => panic! "not an array frame"

end Frame

end MiniRedis
