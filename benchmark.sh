#!/bin/bash

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

function dotest {
    echo "TESTING $1:"
    t=$(/bin/time -f '%e' $2 3>&2 2>&1 1>&3-)
    code=$?
    if [ $code -eq 0 ]; then
        echo "$name,$gitref,$1,$t,$?" >> /home/csun/project/ocaml/results
    else
        echo "$1 exited with $code"
    fi
}

dotest gcc-O0 /tmp/test.gcc-O0.exe
dotest clang-O0 /tmp/test.clang-O0.exe
dotest gcc-O1 /tmp/test.gcc-O1.exe
dotest clang-O1 /tmp/test.clang-O1.exe
dotest gcc-O2 /tmp/test.gcc-O2.exe
dotest clang-O2 /tmp/test.clang-O2.exe
dotest gcc-O3 /tmp/test.gcc-O3.exe
dotest clang-O3 /tmp/test.clang-O3.exe
dotest ocaml "/home/csun/project/ocaml/installed/bin/ocamlrun /tmp/test.ocaml.exe"
dotest ocamlopt /tmp/test.ocamlnative.exe

export OCAMLRUNPARAM='s=4096M,space_overhead=1000000,max_overhead=1000000'

dotest ocaml-nogc "/home/csun/project/ocaml/installed/bin/ocamlrun /tmp/test.ocaml.exe"
dotest ocamlopt-nogc /tmp/test.ocamlnative.exe
