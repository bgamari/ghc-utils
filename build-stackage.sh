#!/bin/bash

# Download cabal.config from, e.g. https://www.stackage.org/nightly-2017-03-29/cabal.config

dir="stackage-build"
mkdir -p $dir

cat > $dir/stackage-test.cabal <<EOF
name: stackage-test
version: 1.0
cabal-version: >= 1.2
build-type: Simple
library
  build-depends:
EOF
sed "s/constraints://" cabal.config | sed 's/^\s\+/    /' | sed '/^--/d' | grep -v ' installed,' >> $dir/stackage-test.cabal
sed "s/constraints://" cabal.config | sed 's/^\s\+//' | grep '==' | cut -d'=' -f1 > $dir/packages.list

cat > $dir/cabal.project <<EOF
packages: .
EOF

cd $dir
cabal new-build -j16 --allow-newer=base,template-haskell,ghc-prim,ghc,directory,time,binary
