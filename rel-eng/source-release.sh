#!/bin/bash -e

make distclean
mk/get-win32-tarballs.sh download all
./boot
./configure
make $@
find -iname '*.o' -delete
find -iname '*.hi' -delete
find -iname '*.dyn_hi' -delete
find -iname '*.dyn_o' -delete
git clean -fxd testsuite/tests
make sdist
