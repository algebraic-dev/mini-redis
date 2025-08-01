/-
Copyright (c) 2025 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import Std.Sync.Channel
import Std.Data.TreeMap

namespace MiniRedis

--- TODO: good broadcast implementation upstream
--- TODO: consider async

/--
Represents a receiver in the broadcast system with unique ID and channel
-/
structure Broadcast.Receiver (α : Type) where
  id : Nat
  channel : Std.Channel α

/--
Manages multiple receivers for broadcasting messages to all subscribers
-/
structure Broadcast (α : Type) where
  channels : Std.TreeMap Nat (Broadcast.Receiver α)
  nextId : Nat
  capacity : Nat

namespace Broadcast

/--
Creates a new broadcast instance with specified channel capacity
-/
def new (capacity : Nat) : Broadcast α := { channels := {}, nextId := 0, capacity }

/--
Sends a message to all registered receivers in the broadcast
-/
def send (b : Broadcast α) (x : α) : BaseIO Nat := do
  b.channels.forM (fun _ recv => recv.channel.sync.send x)
  return b.channels.size

/--
Creates a new receiver and adds it to the broadcast system
-/
def receiver (b : Broadcast α) : BaseIO (Broadcast.Receiver α × Broadcast α) := do
  let receiver := { id := b.nextId, channel := ← Std.Channel.new b.capacity }
  return (
    receiver,
    {
      channels := b.channels.insert b.nextId receiver,
      nextId := b.nextId + 1,
      capacity := b.capacity
    }
  )

/--
Removes a receiver from the broadcast system by its ID
-/
def unsubscribe (b : Broadcast α) (r : Broadcast.Receiver α) : Broadcast α :=
  { b with channels := b.channels.erase r.id }

namespace Receiver

/--
Receives the next message from the receiver's channel
-/
def receive [Inhabited α] (r : Receiver α) : BaseIO (Task α) := do
  r.channel.recv

/--
Gets the selector of the channel.
-/
def receiveSelector [Inhabited α] (r : Receiver α) : Std.Internal.IO.Async.Selector α :=
  r.channel.recvSelector

end Receiver
end Broadcast
end MiniRedis
