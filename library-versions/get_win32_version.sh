#!/usr/bin/env bash

set -e

ghc_version="$1"

get_win32_version() {
    curl --silent "https://downloads.haskell.org/~ghc/${ghc_version}/ghc-${ghc_version}-src.tar.xz" \
        | tar xJf - "ghc-${ghc_version}/libraries/Win32/Win32.cabal" -O \
        | sed -n "s/^[vV]ersion:\s\+\(\w\+\)/\1/p"
}

echo "${ghc_version}   Win32/$(get_win32_version)"
