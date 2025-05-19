/-
Copyright (c) 2025 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import MiniRedis.Clients.Client

open MiniRedis

def pingIt : ClientM Unit := do
  let response := String.fromUTF8! (← ClientM.ping none)
  IO.println s!"Ping got: {response}"
  let buf := String.toUTF8 <| String.mk <| List.replicate 5000 'a'
  let response := String.fromUTF8! (← ClientM.ping buf)
  IO.println s!"Ping with scream got: {response}"
  let response ← ClientM.get "hello"
  IO.println s!"Get on non existent value got: {response}"
  ClientM.set "hello" "world".toUTF8
  let response ← ClientM.get "hello"
  IO.println s!"Get on existing value got: {response}"

def main : IO Unit := do
  IO.println "Starting test"
  let addr := Std.Net.SocketAddressV4.mk (.ofParts 127 0 0 1) 8080
  ClientM.run pingIt addr |>.wait
