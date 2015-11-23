#!/bin/bash -e

make distclean
./boot
./configure
make $@
find -iname '*.o' -delete
find -iname '*.hi' -delete
find -iname '*.dyn_hi' -delete
find -iname '*.dyn_o' -delete
make sdist
