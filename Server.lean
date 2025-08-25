/-
Copyright (c) 2025 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import MiniRedis.Server
import Cli


import MiniRedis.Util.Cancellable
import MiniRedis.Util.Signal
import Std.Internal.Async
import Std.Sync.Notify

open Std.Internal.IO.Async

open MiniRedis
open Cli

def runServerCmd (p : Parsed) : IO UInt32 := do
  let port := p.flag! "port" |>.as! Nat
  let addr := Std.Net.SocketAddressV4.mk (.ofParts 127 0 0 1) port.toUInt16
  ListenerM.run ListenerM.serverLoop addr |>.wait
  return 0

def serverCmd : Cmd := `[Cli|
  "mini-redis" VIA runServerCmd; ["0.1.0"]
  "mini-redis server application"

  FLAGS:
    port : Nat; "The port to host the server on"

  EXTENSIONS:
    defaultValues! #[("port", "8080")]
]

def main (args : List String) : IO UInt32 := do
  serverCmd.validate args
