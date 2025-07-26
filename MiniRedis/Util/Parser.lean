/-
Copyright (c) 2025 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/

/-!
This module provides types for a Parser.
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
Errors that might happen while parsing.
-/
inductive Error where
  /--
  We do not yet have enough data to consume from the input buffer.
  -/
  | incomplete
  /--
  An actual error described by a `String`.
  -/
  | other (err : String)
  deriving Inhabited, BEq, Repr

abbrev ParseM := ExceptT Error <| StateM (ByteArray.Iterator)

namespace ParseM

@[inline]
def throwInvalid : ParseM α :=
  throw <| .other "protocol error; invalid frame format"

def run' (x : ParseM α) (buf : ByteArray.Iterator) : Except Error α :=
  let res := StateT.run' (ExceptT.run x) buf
  res

def run (x : ParseM α) (buf : ByteArray.Iterator) :
    Except Error (α × ByteArray.Iterator) :=
  let (res, it) := StateT.run (ExceptT.run x) buf
  match res with
  | .ok v => .ok (v, it)
  | .error e => .error e

@[inline]
def getU8 : ParseM UInt8 := do
  if (← get).hasNext then
    modifyGet fun it => (it.curr, it.next)
  else
    throw .incomplete

@[inline]
def peekU8 : ParseM UInt8 := do
  let it ← get
  if it.hasNext then
    return it.curr
  else
    throw .incomplete

@[inline]
def skip (count : Nat) : ParseM Unit := do
  if (← get).remainingBytes < count then
    throw .incomplete
  else
    modify fun it => it.forward count

@[inline]
partial def getLine : ParseM ByteArray := do
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
def getDecimal : ParseM Int64 := do
  let line ← getLine
  let some string := String.fromUTF8? line | throwInvalid
  let some int := String.toInt? string | throwInvalid
  if int < Int64.minValue.toInt || int > Int64.maxValue.toInt then
    throwInvalid
  else
    return int.toInt64

@[inline]
def getBytes (count : Nat) : ParseM ByteArray := do
  if (← get).remainingBytes < count then
    throw .incomplete
  else
    modifyGet fun it => (it.array.extract it.pos (it.pos + count), it.forward count)

end ParseM
