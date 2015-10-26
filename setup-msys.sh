#!/bin/bash

boot_ver="7.10.2"
#boot_tarball="ghc-$boot_ver-i386-unknown-mingw32.tar.xz"
boot_tarball="ghc-$boot_ver-x86_64-unknown-mingw32.tar.xz"

pacman -Sy autoconf automake libtool make patch python2 gcc wget vim git tar xz
wget http://downloads.haskell.org/~ghc/$boot_ver/$boot_tarball
tar -Jxf $boot_tarball

git clone https://bgamari@github.com/bgamari/ghc-utils
EXTRA_CONFIGURE_OPTS= CC=$HOME/ghc-$boot_ver/mingw/bin/gcc PATH=$HOME/ghc-$boot_ver/bin:$PATH bash ghc-utils/rel-eng/get-cabal

git clone git://git.haskell.org/ghc
cd ghc
git remote add bgamari https://bgamari@github.com/bgamari/ghc
git remote update
git submodule update --init
./boot
./configure --with-ghc=$HOME/ghc-$boot_ver/bin/ghc --enable-tarballs-autodownload

