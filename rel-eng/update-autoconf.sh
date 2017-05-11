#!/bin/bash -e

tmp=$(tempfile)

curl -s https://git.savannah.gnu.org/cgit/autoconf.git/plain/build-aux/config.guess > $tmp
for i in $(git ls-files | grep config.guess); do
    echo $i
    cp $tmp $i
done

curl -s https://git.savannah.gnu.org/cgit/autoconf.git/plain/build-aux/config.sub > $tmp
for i in $(git ls-files | grep config.sub); do
    echo $i
    cp $tmp $i
done

rm $tmp
