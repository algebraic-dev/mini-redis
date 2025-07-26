/-
Copyright (c) 2025 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/

import MiniRedis.Frame

open MiniRedis

-- Golden tests based on examples from https://redis.io/docs/latest/develop/reference/protocol-spec/#resp-protocol-description

def run (input : String) : Frame := (Prod.fst <$> Frame.parse input.toUTF8.iter).toOption.get!

/-- info: true -/
#guard_msgs in
#eval run "+OK\r\n" == .simple "OK"

/-- info: true -/
#guard_msgs in
#eval run "-Error message\r\n" == .error "Error message"

/-- info: true -/
#guard_msgs in
#eval run ":0\r\n" == .integer 0

/-- info: true -/
#guard_msgs in
#eval run ":1000\r\n" == .integer 1000

/-- info: true -/
#guard_msgs in
#eval run ":-1000\r\n" == .integer (-1000)

/-- info: true -/
#guard_msgs in
#eval run "$5\r\nhello\r\n" == .bulk "hello".toUTF8

/-- info: true -/
#guard_msgs in
#eval run "$0\r\n\r\n" == .bulk "".toUTF8

/-- info: true -/
#guard_msgs in
#eval run "$-1\r\n" == .null

/-- info: true -/
#guard_msgs in
#eval run "*-1\r\n" == .null

/-- info: true -/
#guard_msgs in
#eval run "*0\r\n" == .array #[]

/-- info: true -/
#guard_msgs in
#eval run "*2\r\n$5\r\nhello\r\n$5\r\nworld\r\n" == .array #[.bulk "hello".toUTF8, .bulk "world".toUTF8]

/-- info: true -/
#guard_msgs in
#eval run "*3\r\n:1\r\n:2\r\n:3\r\n" == .array #[.integer 1, .integer 2, .integer 3]

/-- info: true -/
#guard_msgs in
#eval run "*5\r\n:1\r\n:2\r\n:3\r\n:4\r\n$5\r\nhello\r\n" ==
  .array #[.integer 1, .integer 2, .integer 3, .integer 4, .bulk "hello".toUTF8]

/-- info: true -/
#guard_msgs in
#eval run "*2\r\n*3\r\n:1\r\n:2\r\n:3\r\n*2\r\n+Hello\r\n-World\r\n" ==
  .array #[.array #[.integer 1, .integer 2, .integer 3], .array #[.simple "Hello", .error "World"]]
