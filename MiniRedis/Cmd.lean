/-
Copyright (c) 2025 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import MiniRedis.Cmd.Ping
import MiniRedis.Cmd.Get
import MiniRedis.Cmd.Set
import MiniRedis.Cmd.Unknown
import MiniRedis.Cmd.Publish
import MiniRedis.Cmd.Subscribe

namespace MiniRedis

inductive Command
  | ping (ping : Ping)
  | get (get : Get)
  | set (set : Set)
  | unknown (unknown : Unknown)
  | publish (pub : Publish)
  | subscribe (sub : Subscribe)

namespace Command

def ofFrame (f : Frame) : Except String Command :=
  CmdParseM.run go f |>.mapError
    fun
      | .endOfStream => "protocol error; reached end of stream while parsing"
      | .other e => e
where
  go : CmdParseM Command := do
    let commandName := (← CmdParseM.nextString).toLower
    let cmd ←
      match commandName with
      | "ping" => Command.ping <$> OfFrame.ofFrame
      | "get" => Command.get <$> OfFrame.ofFrame
      | "set" => Command.set <$> OfFrame.ofFrame
      | "publish" => Command.publish <$> OfFrame.ofFrame
      | "subscribe" => Command.subscribe <$> OfFrame.ofFrame
      | _ => return Command.unknown <| Unknown.mk commandName

    CmdParseM.finish
    return cmd

def toFrame (cmd : Command) : Frame :=
  match cmd with
  | .ping p => ToFrame.toFrame p
  | .get g => ToFrame.toFrame g
  | .set s => ToFrame.toFrame s
  | .publish p => ToFrame.toFrame p
  | .subscribe p => ToFrame.toFrame p
  | .unknown u => panic! s!"Don't know how to serialize {u.commandName}"

end Command
end MiniRedis
