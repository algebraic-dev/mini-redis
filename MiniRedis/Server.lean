/-
Copyright (c) 2025 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import MiniRedis.Util.Connection
import MiniRedis.Util.Cmd
import MiniRedis.Db

namespace MiniRedis

open Std.Internal.IO.Async

structure HandlerContext where
  db : Database
  connectionLimit : Std.Channel Unit

abbrev HandlerM := ReaderT HandlerContext ConnectionM

namespace HandlerM

def run (x : HandlerM α) (client : TCP.Socket.Client) (db : Database)
    (connectionLimit : Std.Channel Unit) : Async α :=
  ReaderT.run x { db, connectionLimit } |>.run client

partial def handlerLoop : HandlerM Unit := do
  try
    while true do
      let some frame ← ConnectionM.readFrame | break
      let cmd ← Command.ofFrame frame |> IO.ofExcept

      match cmd with
      | .ping p => p.handle
      | .get g => g.handle (← read).db
      | .set s => s.handle (← read).db
      | .unknown u => u.handle
  finally
    -- Relinquish a connection limit token
    (← read).connectionLimit.sync.send ()

end HandlerM

structure ListenerContext where
  listener : TCP.Socket.Server
  db : Database
  connectionLimit : Std.Channel Unit
  -- TODO: shutdown

abbrev ListenerM := ReaderT ListenerContext Async

namespace ListenerM

def run (x : ListenerM α) (addr : Std.Net.SocketAddress) (maxConnections : Nat := 1) :
    Async α := do
  let server ← TCP.Socket.Server.mk
  server.bind addr
  server.listen 4
  let db ← Database.new

  -- Prepare the connection limit mechanism with `maxConnections` connection tokens
  let connectionLimit ← Std.Channel.new (some maxConnections)
  for _ in [:maxConnections] do
    connectionLimit.sync.send ()

  ReaderT.run x { listener := server, db, connectionLimit }

partial def serverLoop : ListenerM Unit := do
  let ctx ← read
  let serverName ← ctx.listener.getSockName
  IO.println s!"Starting server loop on {serverName.ipAddr}:{serverName.port}"

  while true do
    -- Acquire a connection limit token
    await <| ← (← read).connectionLimit.recv
    let client ← await (← ctx.listener.accept)
    let clientName ← client.getPeerName
    IO.println s!"Server: Handling client from {clientName.ipAddr}:{clientName.port}"
    discard <| async (t := AsyncTask) <| HandlerM.handlerLoop.run client ctx.db ctx.connectionLimit

end ListenerM

end MiniRedis
