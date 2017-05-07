#!/bin/bash

# Download cabal.config from, e.g. https://www.stackage.org/nightly-2017-03-29/cabal.config

dir="stackage-build"
mkdir -p $dir

sed "s/constraints://" cabal.config | sed '/^--/d' | sed 's/^\s\+//' | grep '==' > $dir/constraints.list
cat $dir/constraints.list | cut -d'=' -f1 > $dir/packages.list

cat > $dir/stackage-test.cabal <<EOF
name: stackage-test
version: 1.0
cabal-version: >= 1.2
build-type: Simple
library
  build-depends:
EOF
cat $dir/constraints.list | sed 's/^/    /' >> $dir/stackage-test.cabal

mkdir -p $dir/packages
cat > $dir/cabal.project <<EOF
packages: ., packages/
EOF

cd $dir
cabal new-build -j16 --keep-going
