#!/bin/bash -e

make distclean
mk/get-win32-tarballs.sh download all
./boot
./configure
make $@
make sdist
