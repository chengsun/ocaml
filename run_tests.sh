#!/bin/bash

set -e

TESTS=`cd tests; ls -1 *.ml`
#TESTS="test_printf.ml"

for ml in $TESTS; do
    name=${ml%.ml}
    echo ">>> Testing ${name}..."
    cp ./tests/${name}.ml /tmp/test.ml
    ./ocamlc -dlambda -target-liballocs -o /dev/null -g /tmp/test.ml
    gcc -std=c99 -Wall -I. -o /tmp/test ./liballocs.c /tmp/test.c
    diff <(/tmp/test) ./tests/${name}.expected || (echo ">>> Test failed!"; exit 1)
done
