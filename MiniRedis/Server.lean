/-
Copyright (c) 2025 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import MiniRedis.Util.ConnectionLimit
import MiniRedis.Connection
import MiniRedis.Cmd
import MiniRedis.Db

namespace MiniRedis
open Std.Internal.IO.Async

/--
Holds context needed by a connection handler
-/
structure HandlerContext where
  db : Database
  connectionLimit : ConnectionLimit

/--
Handler monad: a reader over HandlerContext, layered on ConnectionM
-/
abbrev HandlerM := ReaderT HandlerContext ConnectionM

namespace HandlerM

/--
Release a connection token back to the pool
-/
def releaseToken : HandlerM Unit := do
  (← read).connectionLimit.release

/--
Acquire a connection token (may block if limit reached)
-/
def adquireToken : HandlerM Unit := do
  (← read).connectionLimit.adquire

/--
Run a handler computation with the given client, db, and connectionLimit
-/
def run
  (x : HandlerM α)
  (client : TCP.Socket.Client)
  (db : Database)
  (connectionLimit : Std.Channel Unit)
  : Async α :=
    ReaderT.run x { db, connectionLimit } |>.run client

/--
Loop that continuously processes client commands
-/
partial def handlerLoop : HandlerM Unit := do
  let signal ← Signal.new

  try
    while true do
      let some frame ← ConnectionM.readFrame signal
        | break

      let cmd ←
        match Command.ofFrame frame with
        | .ok ok =>
          pure ok
        | .error err =>
          ConnectionM.writeFrame <| Frame.error s!"ERR {err}"
          continue

      match cmd with
      | .ping p => p.handle
      | .get g => g.handle (← read).db
      | .set s => s.handle (← read).db
      | .publish p => p.handle (← read).db
      | .subscribe p => p.handle (← read).db
      | .unknown u => u.handle
  catch err =>
    IO.eprintln err
  finally
    .releaseToken

end HandlerM

/--
Holds context needed by the server listener
-/
structure ListenerContext where
  listener : TCP.Socket.Server
  db : Database
  connectionLimit : ConnectionLimit
  shutdown : Signal

/--
Listener monad: a reader over ListenerContext in Async
-/
abbrev ListenerM := ReaderT ListenerContext Async

namespace ListenerM

/--
Run a listener computation on a given address with maxConnections
-/
def run (x : ListenerM α) (addr : Std.Net.SocketAddress) (maxConnections : Nat := 128) : Async α := do
  let server ← TCP.Socket.Server.mk
  server.bind addr
  server.listen 12

  ReaderT.run x { listener := server, db := (← Database.new), connectionLimit := (← ConnectionLimit.new maxConnections), shutdown := (← Signal.new) }

/--
Loop that accepts clients and spawns handler tasks
-/
partial def serverLoop : ListenerM Unit := do
  let ctx ← read
  let serverName ← ctx.listener.getSockName
  IO.println s!"Starting server loop on {serverName.ipAddr}:{serverName.port}"

  while true do
    (← read).connectionLimit.adquire

    let client ← ctx.listener.accept
    let clientName ← client.getPeerName

    IO.println s!"Server: Handling client from {clientName.ipAddr}:{clientName.port}"
    let task := HandlerM.handlerLoop.run client ctx.db ctx.connectionLimit

    background task

end ListenerM
end MiniRedis
