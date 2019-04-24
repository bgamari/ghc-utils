#!/usr/bin/env bash

set -e

tmp=`mktemp -d`
git -C $tmp clone git://git.savannah.gnu.org/config.git
commit=`git -C $tmp/config rev-parse HEAD`
echo "Updating to $commit..."

files=
for i in $(git ls-files | grep config.guess); do
    echo $i
    cp $tmp/config/config.guess $i
    files="$i $files"
done

for i in $(git ls-files | grep config.sub); do
    echo $i
    cp $tmp/config/config.sub $i
    files="$i $files"
done

git commit $files -m "Update autoconf scripts" -m "Scripts taken from autoconf $commit"
rm -Rf $tmp
