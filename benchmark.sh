#!/bin/bash

set -e

name=$1

echo ">>> Benchmarking ${name}..."
cp ./bench/${name}.ml /tmp/test.ml
./ocamlc -dlambda -target-liballocs -o /tmp/test.ocaml.exe -g /tmp/test.ml
./ocamlc -o /tmp/test.ocamlnative.exe -g /tmp/test.ml
#gcc -ggdb -std=c99 -Wall -I. -o /tmp/test /tmp/test.c ./stdlib_liballocs/*.c ./liballocs.c
gcc -O2 -std=c99 -Wall -I. -o /tmp/test.gcc.exe /tmp/test.c ./liballocs.c
clang -O2 -std=c99 -Wall -I. -o /tmp/test.clang.exe /tmp/test.c ./liballocs.c

echo "TESTING GCC:"
time /tmp/test.gcc.exe
echo "TESTING CLANG:"
time /tmp/test.clang.exe
echo "TESTING OCAML:"
time /home/csun/project/ocaml/installed/bin/ocamlrun /tmp/test.ocaml.exe
