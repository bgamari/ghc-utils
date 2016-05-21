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
    echo "  ver                gives the version number (e.g. the name of the tarballs, in the case of"
    echo "                     a release candidate something like 7.10.3.20150820, otherwise just 7.10.3)"
    echo "  rel_name           gives the release name (e.g. in the case of a release candidate 7.10.3-rc2"
    echo "                     otherwise just 7.10.3)"
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
    hash_files="$(find -iname '*.bz2') $(find -iname '*.xz') $(find -iname '*.patch')"

    echo -n "Hashing..."
    sha1sum $hash_files >| SHA1SUMS
    sha256sum $hash_files >| SHA256SUMS
    echo "done"

    # Kill DISPLAY lest pinentry won't work
    DISPLAY=
    eval $(gpg-agent --daemon)
    for i in $hash_files SHA1SUMS SHA256SUMS; do
        if [ -e $i -a $i.sig -nt $i ]; then
            echo "Skipping signing of $i"
            continue
        fi
        echo "Signing $i"
        rm -f $i.sig
        gpg2 --use-agent --detach-sign --local-user="$signing_key" $i
    done
}

function verify() {
    for i in *.sig; do
        echo
        echo Verifying $i
        gpg --verify $i $(basename $i .sig)
    done
}

function upload() {
    verify
    chmod ugo+r,o-w -R .
    rsync --progress -az $rsync_opts . $host:public_html/$rel_name
    chmod ugo-w $(ls *.xz *.bz2)
    # Purge CDN cache
    curl -X PURGE http://downloads.haskell.org/~ghc/$rel_name/
    for i in *; do
        curl -X PURGE http://downloads.haskell.org/~ghc/$rel_name/$i
    done
}

function prepare_docs() {
    rm -Rf docs
    mkdocs="$ghc_tree/distrib/mkDocs/mkDocs"
    if [ ! -e $mkdocs ]; then
        echo "Couldn't find GHC mkDocs at $mkdocs."
        echo "Perhaps you need to override ghc_tree?"
        exit 1
    fi
    windows_bindist="$(ls ghc-*-x86_64-unknown-mingw*.tar.xz | head -n1)"
    linux_bindist="$(ls ghc-*-x86_64-*deb8*.tar.xz | head -n1)"
    $mkdocs $linux_bindist $windows_bindist

    mkdir -p docs/html
    tar -Jxf $linux_bindist
    cp -R ghc-$ver/docs/users_guide/build-html/users_guide docs/html/users_guide
    #cp -R ghc-$ver/utils/haddock/doc/haddock docs/html/haddock
    rm -R ghc-$ver

    tar -Jxf docs/libraries.html.tar.xz -C docs/html
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
