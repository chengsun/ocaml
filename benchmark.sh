#!/bin/bash

set -e

name=$1

echo ">>> Benchmarking ${name}..."
cp ./bench/${name}.ml /tmp/test.ml
OCAMLRUNPARAM=b ./ocamlc -target-liballocs -o /tmp/test.ocaml.exe -g /tmp/test.ml
./ocamlc -o /tmp/test.ocamlnative.exe -g /tmp/test.ml
gcc -ggdb -O2 -march=native -std=c99 -Wall -Wno-unused -I. -o /tmp/test.gcc.exe /tmp/test.c ./stdlib_liballocs/*.c ./liballocs.c
clang -ggdb -O2 -march=native -std=c99 -Wall -Wno-unused -I. -o /tmp/test.clang.exe /tmp/test.c ./stdlib_liballocs/*.c ./liballocs.c

echo "TESTING GCC:"
time /tmp/test.gcc.exe || true
echo "TESTING CLANG:"
time /tmp/test.clang.exe || true
echo "TESTING OCAML:"
time /home/csun/project/ocaml/installed/bin/ocamlrun /tmp/test.ocaml.exe || true
