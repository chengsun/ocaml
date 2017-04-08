#!/bin/bash

set -e

name=$1

echo ">>> Benchmarking ${name}..."
cp ./bench/${name}.ml /tmp/test.ml
./ocamlc -dlambda -target-liballocs -o /tmp/test.ocaml.exe -g /tmp/test.ml
gcc -ggdb -std=c99 -Wall -I. -o /tmp/test ./liballocs.c /tmp/test.c
tmp/test

echo "ALL BENCHES PASSED!!!"
