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

structure Subscribe where
  channels : Array String

namespace Subscribe

def ofFrame : CmdParseM Subscribe := do
  let mut channels := #[]
  channels := channels.push (← CmdParseM.nextString)
  while ← CmdParseM.hasNext do
    channels := channels.push (← CmdParseM.nextString)

  return Subscribe.mk channels

def handle (sub : Subscribe) (db : Database) : ConnectionM Unit := do
  -- TODO: This requires select with monad stacks to implement like rust
  throw <| .userError "Not implemented yet"

def toFrame (sub : Subscribe) : Frame := Id.run do
  let mut frame := Frame.array #[]
  frame := frame.pushBulk "subscribe".toUTF8
  for channel in sub.channels do
    frame := frame.pushBulk channel.toUTF8
  return frame

end Subscribe

end MiniRedis
