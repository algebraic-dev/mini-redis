/-
Copyright (c) 2025 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import MiniRedis.Server

open MiniRedis

def main : IO Unit := do
  IO.println "Starting server"
  let addr := Std.Net.SocketAddressV4.mk (.ofParts 127 0 0 1) 8080
  ListenerM.run ListenerM.serverLoop addr |>.wait
