#!/bin/bash -e

if [ -z "$dest" ]; then
        dest=/opt/ghc
fi
sudo mkdir -p $dest
sudo chown `whoami` $dest

ver="7.8.4"
flavor="i386-unknown-linux-deb7"
url="https://www.haskell.org/ghc/dist/$ver/ghc-$ver-$flavor.tar.xz"
curl -L $url | tar -Jx
cd ghc-$ver

./configure --prefix=$dest/$ver
make install
echo >$dest/$ver/env.sh <<EOF
PATH=$dest/bin:$PATH
LD_LIBRARY_PATH=$dest/lib:$LD_LIBRARY_PATH
echo "Using GHC $ver"
EOF

