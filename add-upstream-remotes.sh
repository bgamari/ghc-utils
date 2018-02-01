#!/usr/bin/env bash

set -e

add() {
    submod=$1
    upstream=$2
    if [ -z "$upstream" ]; then
        upstream=$(grep -v '^#' packages | grep $submod | awk '{print $4}')
    fi
    if [ -z "$upstream" -o "$upstream" = "-" ]; then
        echo "$submod: unknown upstream"
    elif git -C $submod remote | grep upstream >/dev/null; then
        echo "$submod: upstream remote already exists for $submod"
    else
        echo "$submod: upstream = $upstream"
        git -C $submod remote add upstream $upstream
        git -C $submod remote update upstream
    fi
}

for s in `git submodule | awk '{print $2'}`; do
    add $s
done
add utils/hpc git@github.com:haskell/hpc

#add libraries/Cabal git://github.com/haskell/Cabal
#add libraries/bytestring git://github.com/haskell/bytestring
#add libraries/containers git://github.com/haskell/containers
#add libraries/haskeline git://github.com/judah/haskeline
#add libraries/win32 git://github.com/haskell/Win32
#add libraries/binary git@github.com:kolmodin/binary
#add libraries/directory git://github.com/haskell/directory
#add libraries/process git://github.com/haskell/process
#add libraries/time git://github.com/haskell/time
#add libraries/unix git@github.com:haskell/unix
#add libraries/primitive git@github.com:haskell/primitive
#add utils/haddock git@github.com:haskell/haddock

