/-
Copyright (c) 2025 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import MiniRedis.Clients.Client

open MiniRedis

def pingIt : ClientM (Array String) := do
  let response1 := String.fromUTF8! (← ClientM.ping none)
  let response2 := String.fromUTF8! (← ClientM.ping none)
  let buf := String.toUTF8 <| String.mk <| List.replicate 5000 'a'
  let response3 := String.fromUTF8! (← ClientM.ping buf)
  let response4 := String.fromUTF8! (← ClientM.ping none)
  return #[response1, response2, response3, response4]

def main : IO Unit := do
  IO.println "Starting ping"
  let addr := Std.Net.SocketAddressV4.mk (.ofParts 127 0 0 1) 8080
  let responses ← ClientM.run pingIt addr |>.wait
  IO.println s!"Responses: {responses}"
