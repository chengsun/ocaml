#!/bin/bash

set -e

TESTS=${1-`cd tests; ls -1 *.ml`}
#TESTS="test_printf.ml"

for ml in $TESTS; do
    name=${ml%.ml}
    echo ">>> Testing ${name}..."
    cp ./tests/${name}.ml /tmp/test.ml
    ./ocamlc -dlambda -target-liballocs -o /dev/null -g /tmp/test.ml
    gcc -ggdb -std=c99 -Wall -I. -o /tmp/test ./liballocs.c /tmp/test.c
    output=$(/tmp/test 2>&1) || true
    echo "$output" | diff -u /dev/stdin ./tests/${name}.expected || (echo ">>> Test failed!"; exit 1)
done

echo "ALL TESTS PASSED!!!"
