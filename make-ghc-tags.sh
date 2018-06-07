#!/usr/bin/env bash

set -e

args=
build_tags() {
    d=$1
    if [ -d $d ]; then
        echo "generating tags for $d..."
        pushd $d
        hasktags -x -o TAGS -e .
        popd
        args="$args --include $d/TAGS"
    fi
}

build_tags compiler
build_tags ghc
build_tags libraries/base
build_tags libraries/ghc-boot
build_tags libraries/ghc-prim
build_tags libraries/ghci
build_tags libraries/hoopl
build_tags iserv

pushd rts
etags $(find -iname '*.c')
args="$args --include rts/TAGS"
popd

etags $args
