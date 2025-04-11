#!/usr/bin/env bash

ZIG_CMD=$(command -v zigdemu || command -v zig) || { echo "Error: Zig not detected"; exit 1; }

command -v valgrind &> /dev/null || { echo "Error: valgrind not detected"; exit 1; }

$ZIG_CMD build -Doptimize=ReleaseSafe -Dcpu=x86_64_v3
valgrind --tool=callgrind ./zig-out/bin/lunazitic $@
