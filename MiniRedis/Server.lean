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

abbrev HandlerM := ReaderT HandlerContext ConnectionM

namespace HandlerM

def run (x : HandlerM α) (client : TCP.Socket.Client) (db : Database) : Async α :=
  ReaderT.run x { db } |>.run client

partial def handlerLoop : HandlerM Unit := do
  while true do
    let some frame ← ConnectionM.readFrame | return ()
    let cmd ← Command.ofFrame frame |> IO.ofExcept

    match cmd with
    | .ping p => p.handle
    | .get g => g.handle (← read).db
    | .set s => s.handle (← read).db
    | .unknown u => u.handle

end HandlerM

structure ListenerContext where
  listener : TCP.Socket.Server
  db : Database
  -- TODO: connection limit
  -- TODO: shutdown

abbrev ListenerM := ReaderT ListenerContext Async

namespace ListenerM

def run (x : ListenerM α) (addr : Std.Net.SocketAddress) : Async α := do
  let server ← TCP.Socket.Server.mk
  server.bind addr
  server.listen 4
  let db ← Database.new
  ReaderT.run x { listener := server, db }

partial def serverLoop : ListenerM Unit := do
  let ctx ← read

  while true do
    let client ← await (← ctx.listener.accept)
    -- TODO: run handler async?
    HandlerM.run HandlerM.handlerLoop client ctx.db

end ListenerM

end MiniRedis
