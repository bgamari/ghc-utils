#!/bin/bash -e

make distclean
if [[ $(git clean -dn | wc -l) > 1 ]]; then
    echo "Dirty tree:"
    git clean -dn
    exit 1;
fi
git clean -dxf

mk/get-win32-tarballs.sh download all
./boot
./configure
make $@

find -iname '*.o' -delete
find -iname '*.hi' -delete
find -iname '*.dyn_hi' -delete
find -iname '*.dyn_o' -delete
git clean -fxd testsuite/tests
find -type f -executable -exec file -i '{}' \; | grep 'x-executable; charset=binary' | cut -d: -f1 | xargs rm

make sdist
