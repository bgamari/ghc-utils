#!/bin/bash -e

# This is a script for preparing and uploading a release of GHC.
#
# Usage,
#   1. Update $ver
#   2. Set $signing_key to your key id (prefixed with '=')
#   3. Update $ghc_tree to point at a source working tree of the version being
#      released.
#   4. Create a directory and place the source and binary tarballs there
#   5. Run this script from that directory
#
# You can also invoke the script with an argument to perform only
# a subset of the usual release,
#
#   upload.sh compress_to_xz         produce xz tarballs from bzip2 tarballs
#
#   upload.sh gen_hashes             generate signed hashes of the release
#                                    tarballs
#
#   upload.sh prepare_docs           prepare the documentation directory
#
#   upload.sh upload                 upload the tarballs and documentation
#                                    to downloads.haskell.org
#
# Prerequisites: moreutils

signing_key="=Benjamin Gamari <ben@well-typed.com>"


# A working directory of the version being packaged
if [ -z "$ghc_tree" ]; then
    ghc_tree=/opt/exp/ghc/ghc-7.10
fi

host="downloads.haskell.org"

usage() {
    echo "Usage: [rel_name=<name>] ver=7.10.3-rc2 ghc_tree=/path/to/ghc/tree $0 <action>"
    echo
    echo "where,"
    echo "  ghc-tree           gives the location of GHC source tree"
    echo "  ver                gives the official version number (e.g. the name of the tarballs)"
    echo "  rel_name           gives the version name (e.g. in the case of a release candidate, 7.10.3-rc2)"
    echo "and <action> is one of,"
    echo "  [nothing]          do everything below"
    echo "  compress_to_xz     produce xz tarballs from bzip2 tarballs"
    echo "  gen_hashes         generated signed hashes of the release tarballs"
    echo "  prepare_docs       prepare the documentation directory"
    echo "  upload             upload the tarballs and documentation to downloads.haskell.org"
    echo
}

if [ -z "$ver" ]; then
    usage
    exit 1
fi
if [ -z "$rel_name" ]; then
    rel_name="$ver"
fi

function gen_hashes() {
    sha1sum *.bz2 *.xz >| SHA1SUMS
    sha256sum *.bz2 *.xz >| SHA256SUMS
    for i in SHA1SUMS SHA256SUMS; do
        gpg --detach-sign --local-user="$signing_key" $i
    done
}

function upload() {
    chmod ugo+r,o-w -R .
    rsync --progress -az $rsync_opts . $host:public_html/$rel_name
}

function prepare_docs() {
    rm -Rf docs
    mkdocs="$ghc_tree/distrib/mkDocs/mkDocs"
    if [ ! -e $mkdocs ]; then
        echo "Couldn't find GHC mkDocs at $mkdocs."
        echo "Perhaps you need to override ghc_tree?"
        exit 1
    fi
    windows_bindist="$(ls ghc-$ver-x86_64-unknown-mingw*.tar.bz2 | head -n1)"
    linux_bindist="$(ls ghc-$ver-x86_64-unknown-linux-*.tar.bz2 | head -n1)"
    $mkdocs $linux_bindist $windows_bindist

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
        echo "Recompressing $f.bz2 to $f.xz"
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
