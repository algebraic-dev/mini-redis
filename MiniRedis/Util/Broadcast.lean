/-
Copyright (c) 2025 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import Std.Sync.Channel
import Std.Data.TreeMap

namespace MiniRedis

-- TODO: good broadcast implementation upstream

structure Broadcast.Receiver (α : Type) where
  id : Nat
  channel : Std.Channel α

structure Broadcast (α : Type) where
  channels : Std.TreeMap Nat (Broadcast.Receiver α)
  nextId : Nat
  capacity : Nat

namespace Broadcast

def new (capacity : Nat) : Broadcast α := { channels := {}, nextId := 0, capacity }

-- TODO: consider async
def send (b : Broadcast α) (x : α) : BaseIO Unit := do
  b.channels.forM (fun key recv => recv.channel.sync.send x)

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

def unsubscribe (b : Broadcast α) (r : Broadcast.Receiver α) : Broadcast α :=
  { b with channels := b.channels.erase r.id }

namespace Receiver

def receive [Inhabited α] (r : Receiver α) : BaseIO (Task α) := do
  r.channel.recv

end Receiver

end Broadcast

end MiniRedis
