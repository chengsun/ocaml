#!/bin/bash

set -e

TESTS=${1-`cd tests; ls -1 *.ml`}
#TESTS="test_printf.ml"

for ml in $TESTS; do
    name=${ml%.ml}

    if [[ "$name" = "test_1" ]]; then continue; fi
    if [[ "$name" = "test_overapplication" ]]; then continue; fi

    echo ">>> Testing ${name}..."
    cp ./tests/${name}.ml /tmp/test.ml
    OCAMLRUNPARAM=b ./ocamlc -target-liballocs -o /dev/null -g /tmp/test.ml
    gcc -ggdb -std=c99 -Wall -Wno-unused -I. -o /tmp/test ./liballocs.c /tmp/test.c -lm
    output=$(/tmp/test 2>&1) || true
    echo "$output" | diff -u /dev/stdin ./tests/${name}.expected || (echo ">>> Test failed!"; exit 1)
done

echo "ALL TESTS PASSED!!!"
