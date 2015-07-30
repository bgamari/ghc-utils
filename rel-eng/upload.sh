#!/bin/bash -e

# This is a script for preparing and uploading a release of GHC.
#
# Usage,
#   1. Update $ver
#   2. Set $signing_key to your key id
#   3. Update $ghc_tree to point at a source working tree of the version being
#      released.
#   4. Create a directory and place the source and binary tarballs there
#   5. Run this script from that directory

ver=7.10.2
signing_key="=Benjamin Gamari <ben@well-typed.com>"
# A working directory of the version being packaged
ghc_tree=/opt/exp/ghc/ghc-7.10

host=downloads.haskell.org
windows_bindist="ghc-$ver-x86_64-unknown-mingw32.tar.bz2"
linux_bindist="ghc-$ver-x86_64-unknown-linux-deb7.tar.bz2"

function gen_hashes() {
    sha1sum *.bz2 *.xz >| SHA1SUMS
    sha256sum *.bz2 *.xz >| SHA256SUMS
    for i in SHA1SUMS SHA256SUMS; do
        gpg --detach-sign --local-user="$signing_key" $i
    done
}

function upload() {
    chmod ugo+r,o-w -R .
    rsync --progress -az . $host:public_html/$ver
}

function prepare_docs() {
    rm -R docs
    $ghc_tree/distrib/mkDocs/mkDocs $linux_bindist $windows_bindist

    mkdir -p docs/html
    tar -jxf $linux_bindist
    cp -R ghc-$ver/docs/users_guide/users_guide docs/html/users_guide
    cp -R ghc-$ver/utils/haddock/doc/haddock docs/html/haddock
    rm -R ghc-$ver

    tar -jxf docs/libraries.html.tar.bz2 -C docs/html
    mv docs/index.html docs/html
}

function compress_to_xz() {
    for f in $(combine <(basename -s .bz2 *.bz2) not <(basename -s .xz *.xz)); do
        bunzip2 -c $f.bz2 | xz -c - > $f.xz
    done
}

if [ "x$1" == "x" ]; then
    gen_hashes
    prepare_docs
    upload
else
    $@
fi
