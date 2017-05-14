#!/bin/bash

set -e

./build_liballocs.sh

if [ -f $1 ]; then
    path=$1
    name=$(basename -s .ml $1)
else
    name=$1
    path=./bench/${name}.ml
fi
gitref=$(git rev-parse --short HEAD)

echo ">>> Benchmarking ${name}..."
cp "$path" /tmp/test.ml
OCAMLRUNPARAM=b ./ocamlc -target-liballocs -o /tmp/test.ocaml.exe -g /tmp/test.ml
ocamlopt -o /tmp/test.ocamlnative.exe -g /tmp/test.ml
gcc -O0 -std=c99 -Wall -Wno-unused -I. -o /tmp/test.gcc-O0.exe /tmp/test.c ./stdlib_liballocs/*.c ./liballocs_runtime.c -lm
gcc -O1 -std=c99 -Wall -Wno-unused -I. -o /tmp/test.gcc-O1.exe /tmp/test.c ./stdlib_liballocs/*.c ./liballocs_runtime.c -lm
gcc -O2 -std=c99 -Wall -Wno-unused -I. -o /tmp/test.gcc-O2.exe /tmp/test.c ./stdlib_liballocs/*.c ./liballocs_runtime.c -lm
gcc -O3 -std=c99 -Wall -Wno-unused -I. -o /tmp/test.gcc-O3.exe /tmp/test.c ./stdlib_liballocs/*.c ./liballocs_runtime.c -lm
clang -O0 -std=c99 -Wall -Wno-unused -I. -o /tmp/test.clang-O0.exe /tmp/test.c ./stdlib_liballocs/*.c ./liballocs_runtime.c -lm
clang -O1 -std=c99 -Wall -Wno-unused -I. -o /tmp/test.clang-O1.exe /tmp/test.c ./stdlib_liballocs/*.c ./liballocs_runtime.c -lm
clang -O2 -std=c99 -Wall -Wno-unused -I. -o /tmp/test.clang-O2.exe /tmp/test.c ./stdlib_liballocs/*.c ./liballocs_runtime.c -lm
clang -O3 -std=c99 -Wall -Wno-unused -I. -o /tmp/test.clang-O3.exe /tmp/test.c ./stdlib_liballocs/*.c ./liballocs_runtime.c -lm

echo "TESTING GCC:"
t=$(/bin/time -f '%e' /tmp/test.gcc-O0.exe 2>&1 1>/dev/null)
echo "$name,$gitref,gcc-O0,$t,$?" >> /home/csun/project/ocaml/results

echo "TESTING CLANG:"
t=$(/bin/time -f '%e' /tmp/test.clang-O0.exe 2>&1 1>/dev/null)
echo "$name,$gitref,clang-O0,$t,$?" >> /home/csun/project/ocaml/results

echo "TESTING GCC:"
t=$(/bin/time -f '%e' /tmp/test.gcc-O1.exe 2>&1 1>/dev/null)
echo "$name,$gitref,gcc-O1,$t,$?" >> /home/csun/project/ocaml/results

echo "TESTING CLANG:"
t=$(/bin/time -f '%e' /tmp/test.clang-O1.exe 2>&1 1>/dev/null)
echo "$name,$gitref,clang-O1,$t,$?" >> /home/csun/project/ocaml/results

echo "TESTING GCC:"
t=$(/bin/time -f '%e' /tmp/test.gcc-O2.exe 2>&1 1>/dev/null)
echo "$name,$gitref,gcc-O2,$t,$?" >> /home/csun/project/ocaml/results

echo "TESTING CLANG:"
t=$(/bin/time -f '%e' /tmp/test.clang-O2.exe 2>&1 1>/dev/null)
echo "$name,$gitref,clang-O2,$t,$?" >> /home/csun/project/ocaml/results

echo "TESTING GCC:"
t=$(/bin/time -f '%e' /tmp/test.gcc-O3.exe 2>&1 1>/dev/null)
echo "$name,$gitref,gcc-O3,$t,$?" >> /home/csun/project/ocaml/results

echo "TESTING CLANG:"
t=$(/bin/time -f '%e' /tmp/test.clang-O3.exe 2>&1 1>/dev/null)
echo "$name,$gitref,clang-O3,$t,$?" >> /home/csun/project/ocaml/results

echo "TESTING OCAML (bytecode):"
t=$(/bin/time -f '%e' /home/csun/project/ocaml/installed/bin/ocamlrun /tmp/test.ocaml.exe 2>&1 1>/dev/null)
echo "$name,$gitref,ocaml,$t,$?" >> /home/csun/project/ocaml/results

echo "TESTING OCAML (native):"
t=$(/bin/time -f '%e' /tmp/test.ocamlnative.exe 2>&1 1>/dev/null)
echo "$name,$gitref,ocamlopt,$t,$?" >> /home/csun/project/ocaml/results

export OCAMLRUNPARAM='s=4096M'

echo "TESTING OCAML (bytecode no GC):"
t=$(/bin/time -f '%e' /home/csun/project/ocaml/installed/bin/ocamlrun /tmp/test.ocaml.exe 2>&1 1>/dev/null)
echo "$name,$gitref,ocaml-nogc,$t,$?" >> /home/csun/project/ocaml/results

echo "TESTING OCAML (native no GC):"
t=$(/bin/time -f '%e' /tmp/test.ocamlnative.exe 2>&1 1>/dev/null)
echo "$name,$gitref,ocamlopt-nogc,$t,$?" >> /home/csun/project/ocaml/results
