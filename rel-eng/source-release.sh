#!/bin/bash -e

make distclean
./boot
./configure
make $@
make sdist
