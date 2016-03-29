#!/bin/bash -e

args=
build_tags() {
    d=$1
    echo "generating tags for $d..."
    pushd $d
    hasktags -x -o TAGS -e .
    popd
    args="$args --include $d/TAGS"
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
