/-
Copyright (c) 2025 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import MiniRedis.Util.Frame
import MiniRedis.Util.Connection

namespace MiniRedis

structure Unknown where
  commandName : String

namespace Unknown

def handle (u : Unknown) : ConnectionM Unit := do
  ConnectionM.writeFrame <| Frame.error s!"ERR unknown command {u.commandName}"

end Unknown
