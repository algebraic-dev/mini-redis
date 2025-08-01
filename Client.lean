/-
Copyright (c) 2025 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import MiniRedis.Client
import Cli

open MiniRedis
open Cli

open Std.Internal.IO.Async

def sockAddrOfCli (p : Parsed) : IO Std.Net.SocketAddress := do
  let port := p.flag! "port" |>.as! Nat
  let some hostname := p.flag! "hostname" |>.as! String |> Std.Net.IPv4Addr.ofString
    | throw <| .userError "Invalid IPv4 as hostname"
  return .v4 <| .mk hostname port

def runPingCmd (p : Parsed) : IO UInt32 := do
  let addr ← sockAddrOfCli p
  ClientM.run (pingIt p) addr |>.wait
  return 0
where
  pingIt (p : Parsed) : ClientM Unit := do
    let msg := p.flag? "msg" |>.map (·.as! String |>.toUTF8)
    let response := String.fromUTF8! (← ClientM.ping msg)
    IO.println response

def runGetCmd (p : Parsed) : IO UInt32 := do
  let addr ← sockAddrOfCli p
  ClientM.run (getIt p) addr |>.wait
  return 0
where
  getIt (p : Parsed) : ClientM Unit := do
    let key := p.positionalArg! "key" |>.as! String
    let response ← ClientM.get key
    match response with
    | some res => IO.println <| String.fromUTF8! res
    | none => IO.println "null"

def runSetCmd (p : Parsed) : IO UInt32 := do
  let addr ← sockAddrOfCli p
  ClientM.run (setIt p) addr |>.wait
  return 0
where
  setIt (p : Parsed) : ClientM Unit := do
    let key := p.positionalArg! "key" |>.as! String
    let value := p.positionalArg! "value" |>.as! String |>.toUTF8
    let timeout :=
      p.flag? "timeout"
        |>.map (·.as! Nat |> Std.Time.Millisecond.Offset.ofNat |> .ofMillisecond)
    ClientM.set key value timeout

def pingCmd : Cmd := `[Cli|
  ping VIA runPingCmd; "Ping the server, maybe with a custom message"

  FLAGS:
    hostname; "Hostname of the server to connect to"
    port : Nat; "Port of the server to connect to"
    msg : String; "A message for the server to echo"

  EXTENSIONS:
    defaultValues! #[("hostname", "127.0.0.1"), ("port", "8080")]
]

def getCmd : Cmd := `[Cli|
  get VIA runGetCmd; "Get a key from the server"

  FLAGS:
    hostname; "Hostname of the server to connect to"
    port : Nat; "Port of the server to connect to"

  ARGS:
    key : String; "The key to fetch"

  EXTENSIONS:
    defaultValues! #[("hostname", "127.0.0.1"), ("port", "8080")]
]

def setCmd : Cmd := `[Cli|
  set VIA runSetCmd; "Set a key on the server"

  FLAGS:
    timeout : Nat; "Timeout for the key in milliseconds"
    hostname; "Hostname of the server to connect to"
    port : Nat; "Port of the server to connect to"

  ARGS:
    key : String; "The key to set"
    value : String; "The value for the key"

  EXTENSIONS:
    defaultValues! #[("hostname", "127.0.0.1"), ("port", "8080")]
]


def runClientCmd (p : Parsed) : IO UInt32 := do
  p.cmd.toFullCmd.printHelp
  return 0

def clientCmd : Cmd := `[Cli|
  "mini-redis-client" VIA runClientCmd; ["0.1.0"]
  "mini-redis client application"

  SUBCOMMANDS:
    pingCmd;
    getCmd;
    setCmd
]

def main (args : List String) : IO UInt32 := do
  clientCmd.validate args
  --IO.println "Starting test"
  --let addr := Std.Net.SocketAddressV4.mk (.ofParts 127 0 0 1) 8080
  --ClientM.run pingIt addr |>.wait
