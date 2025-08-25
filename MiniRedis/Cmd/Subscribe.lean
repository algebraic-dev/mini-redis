/-
Copyright (c) 2025 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import MiniRedis.Frame
import MiniRedis.Cmd.Basic
import MiniRedis.Cmd.Unknown
import MiniRedis.Cmd.Ping
import MiniRedis.Util.Cancellable
import MiniRedis.Connection
import MiniRedis.Db
import Std.Sync.StreamMap

/-!
This module implements parsing and interpretation of the Redis `SUBSCRIBE` and `UNSUBSCRIBE` commands.
Reference: https://redis.io/docs/latest/commands/subscribe and https://redis.io/docs/latest/commands/unsubscribe
-/

namespace MiniRedis
open Std Internal.IO.Async

/--
Represents a command to subscribe to a list of channels. Used to start receiving input or messages
from the specified channels.
-/
structure Subscribe where
  channels : Array String

instance : OfFrame Subscribe where
  ofFrame := do
    let mut channels := #[].push (← CmdParseM.nextString)

    while ← CmdParseM.hasNext do
      channels := channels.push (← CmdParseM.nextString)

    return Subscribe.mk channels

instance : ToFrame Subscribe where
 toFrame sub := Id.run do
    let mut frame := Frame.array #[]
    frame := frame.pushBulk "subscribe".toUTF8
    for channel in sub.channels do
      frame := frame.pushBulk channel.toUTF8
    return frame

/--
Represents a command to unsubscribe from a list of channels. Used to stop receiving input or messages
from the specified channels.
-/
structure Unsubscribe where
  channels : Array String

instance : OfFrame Unsubscribe where
  ofFrame := do
    let mut channels := #[]
    channels := channels.push (← CmdParseM.nextString)
    while ← CmdParseM.hasNext do
      channels := channels.push (← CmdParseM.nextString)
    return Unsubscribe.mk channels

namespace Message

/--
Represents a message received from a subscribed channel. Contains the channel name
and the message data as a byte array.
-/
structure ChannelMessage where
  name : String
  data : ByteArray

instance : ToFrame ChannelMessage where
  toFrame subs := Id.run do
    let mut frame := Frame.array #[]
    frame := frame.pushBulk "message".toUTF8
    frame := frame.pushBulk subs.name.toUTF8
    frame := frame.pushBulk subs.data
    return frame

/--
Represents a subscription confirmation message. Contains the channel name
and the current number of subscribers to that channel.
-/
structure Subscription where
  name : String
  numSubs : Nat

instance : ToFrame Subscription where
  toFrame subs := Id.run do
    let mut frame := Frame.array #[]
    frame := frame.pushBulk "subscribe".toUTF8
    frame := frame.pushBulk subs.name.toUTF8
    frame := frame.pushInt subs.numSubs.toInt64
    return frame

/--
Represents an unsubscription confirmation message. Contains the channel name
and the remaining number of subscribers to that channel.
-/
structure Unsubscription where
 name : String
 numSubs : Nat

instance : ToFrame Unsubscription where
  toFrame message := Id.run do
    let mut frame := Frame.array #[]
    frame := frame.pushBulk "unsubscribe".toUTF8
    frame := frame.pushBulk message.name.toUTF8
    frame := frame.pushInt message.numSubs.toInt64
    return frame

end Message

namespace Subscribe

-- Type for types of commands that can be used inside pub/sub mode.
private inductive SubscribeCmds
  | subscribe (subscribe : Subscribe)
  | unsubscribe (unsubscribe : Unsubscribe)
  | ping (ping : Ping)
  | unknown (unsubscribe : Unknown)

private inductive SubscribeEvent
  | channel (name : String) (data : ByteArray)
  | command (frame : Except IO.Error (Option Frame))

private def ChannelSet := StreamMap ByteArray

-- Subscribes the current connection to a channel set.
private def subscribe (db : Database) (set : ChannelSet) (channel : String) : ConnectionM ChannelSet := do
  if set.contains channel then
    return set

  ConnectionM.writeFrame <| ToFrame.toFrame (Message.Subscription.mk channel (set.size + 1))

  let subscribed ← db.subscribe channel

  return set.register channel subscribed

-- Unsubscribes the current connection to a channel set.
private def unsubscribe (set : ChannelSet) (channel : String) : ConnectionM ChannelSet := do
  if ¬set.contains channel then
    return set

  ConnectionM.writeFrame <| ToFrame.toFrame (Message.Unsubscription.mk channel (set.size + 1))

  return set.unregister channel

-- Parses a frame
private def parseFrame (frame : Frame) : ConnectionM SubscribeCmds := do
  let go : CmdParseM SubscribeCmds := do
    let commandName := (← CmdParseM.nextString).toLower

    let cmd ←
      match commandName with
      | "subscribe" => SubscribeCmds.subscribe <$> OfFrame.ofFrame
      | "unsubscribe" => SubscribeCmds.unsubscribe <$> OfFrame.ofFrame
      | "ping" => SubscribeCmds.ping <$> OfFrame.ofFrame
      | _ => return (SubscribeCmds.unknown (Unknown.mk commandName))

    CmdParseM.finish

    return cmd

  let result :=
    CmdParseM.run go frame |>.mapError
    fun
      | .endOfStream => "protocol error; reached end of stream while parsing"
      | .other e => e

  IO.ofExcept result

/--
Runs a `Subscribe` with a `Database`.
-/
def handle (sub : Subscribe) (db : Database) : ConnectionM Unit := do
  let mut set ← sub.channels.foldlM (subscribe db) StreamMap.empty

  while true do
    let channelSelector := Selectable.case set.selector (pure ∘ AsyncTask.pure ∘ Function.uncurry SubscribeEvent.channel)
    let commandSelector := Selectable.case (← cancellableSelector ConnectionM.readFrame2) (pure ∘ pure ∘ SubscribeEvent.command)

    let result ← Selectable.one #[channelSelector, commandSelector]

    match result with
    | .channel name data =>
      ConnectionM.writeFrame <| ToFrame.toFrame (Message.ChannelMessage.mk name data)

    | .command frame =>
      let some frame ← IO.ofExcept frame
        -- Connection ended for some reason.
        | break

      match ← parseFrame frame with
      | .unknown c => c.handle
      | .ping c => c.handle
      | .subscribe s => do set ← s.channels.foldlM (subscribe db) set
      | .unsubscribe u => do set ← u.channels.foldlM unsubscribe set

  set.close

end Subscribe
end MiniRedis
