#!/bin/bash

set -e

make -j4 world
./ocamlc -dlambda -target-liballocs -g ./stdlib_liballocs/list.ml
