#!/bin/bash

set -e

make -j4 world
cd stdlib_liballocs
../ocamlc -strict-sequence -w +32+33..39+50 -g -warn-error A -bin-annot -nopervasives -nostdlib -target-liballocs -c ./pervasives.mli
../ocamlc -strict-sequence -w +32+33..39+50 -g -warn-error A -bin-annot -nopervasives -nostdlib -target-liballocs -c ./pervasives.ml

../ocamlc -strict-sequence -w +32+33..39+50 -g -warn-error A -bin-annot -nostdlib -target-liballocs -c ./std_exit.ml

../ocamlc -strict-sequence -w +32+33..39+50 -g -warn-error A -bin-annot -nostdlib -target-liballocs -c ./list.mli

../ocamlc -strict-sequence -w +32+33..39+50 -g -warn-error A -bin-annot -nostdlib -target-liballocs -c ./list.ml
