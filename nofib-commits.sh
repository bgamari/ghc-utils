#!/bin/bash -e

mkdir -p logs

do_commit() {
    commit=$1
    sha=`git rev-parse $commit`
    echo "Building $1 ($commit)..."
    git checkout $commit
    git submodule update --init
    ./validate --build-only >| logs/build-$sha.log

    echo "Nofibbing $1 ($commit)..."
    pushd nofib
    make clean
    make boot
    make >| logs/nofib-$sha.log 2>&1
}

for commit in $@; do
    do_commit || echo "commit $commit failed"
done
