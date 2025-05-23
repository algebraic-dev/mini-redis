#!/bin/bash
cd .lake/build/bin

set -x -e
./mini-redis-client ping
./mini-redis-client ping --msg "hello world"
./mini-redis-client get hello
./mini-redis-client set hello world
./mini-redis-client get hello
./mini-redis-client set hello world --timeout 2000
sleep 1
./mini-redis-client get hello
sleep 2
./mini-redis-client get hello
./mini-redis-client set hello world --timeout 5000
./mini-redis-client set short sleep --timeout 3000
sleep 2
./mini-redis-client get short
./mini-redis-client get hello
sleep 2
./mini-redis-client get short
./mini-redis-client get hello
sleep 2
./mini-redis-client get hello

