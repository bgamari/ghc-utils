#!/usr/bin/env bash

set -e

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

# Infer release name from directory name
if [ -z "$rel_name" ]; then
    rel_name="$(basename $(pwd))"
fi

# Infer version from tarball names
if [ -z "$ver" ]; then
    ver="$(ls ghc-*.tar.* | sed -ne 's/ghc-\([0-9]\+\.[0-9]\+\.[0-9]\+\(\.[0-9]\+\)\?\).\+/\1/p' | head -n1)"
    if [ -z "$ver" ]; then echo "Failed to infer \$ver"; exit 1; fi
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
    echo "  fetch_s3           fetch artifacts from S3"
    echo "  [nothing]          do everything below"
    echo "  compress_to_xz     produce xz tarballs from bzip2 tarballs"
    echo "  gen_hashes         generated hashes of the release tarballs"
    echo "  sign               sign hashes of the release tarballs"
    echo "  prepare_docs       prepare the documentation directory"
    echo "  upload             upload the tarballs and documentation to downloads.haskell.org"
    echo "  purge_file file    purge a given file from the CDN"
    echo "  verify             verify the signatures in this directory"
    echo
}

if [ -z "$ver" ]; then
    usage
    exit 1
fi
if [ -z "$rel_name" ]; then
    rel_name="$ver"
fi

function fetch_s3() {
    declare -A builds
    builds[x86_64-deb8-linux]=validate-x86_64-linux
    builds[i386-deb8-linux]=validate-i386-linux
    builds[x86_64-fedora27-linux]=validate-x86_64-fedora
    builds[x86_64-darwin]=validate-x86_64-darwin

    for platform in "${!builds[@]}"; do
        out_name="ghc-$ver-$platform.tar.xz"
        if [ ! -e "$out_name" ]; then
            name="${builds[$platform]}"
            s3cmd get s3://ghc-artifacts/releases/$name/ghc-$rel_name/bindist.tar.xz
            mv bindist.tar.xz $out_name
        fi
    done
}

# returns the set of files that must have hashes generated.
function hash_files() {
    echo $(find -maxdepth 1 -iname '*.bz2') $(find -maxdepth 1 -iname '*.xz') $(find -maxdepth 1 -iname '*.patch')
}

function gen_hashes() {
    echo -n "Hashing..."
    sha1sum $(hash_files) >| SHA1SUMS &
    sha256sum $(hash_files) >| SHA256SUMS &
    wait
    echo "done"
}

function sign() {
    # Kill DISPLAY lest pinentry won't work
    DISPLAY=
    eval $(gpg-agent --daemon)
    for i in $(hash_files) SHA1SUMS SHA256SUMS; do
        if [ -e $i -a -e $i.sig -a $i.sig -nt $i ]; then
            echo "Skipping signing of $i"
            continue
        elif [ -e $i.sig ] && gpg2 --verify $i.sig; then
            # Don't resign if current signature is valid
            touch $i.sig
            continue
        fi
        echo "Signing $i"
        rm -f $i.sig
        gpg2 --use-agent --detach-sign --local-user="$signing_key" $i
    done
}

function verify() {
    if [ $(find -iname '*.sig' | wc -l) -eq 0 ]; then
        echo "No signatures to verify"
        return
    fi

    for i in *.sig; do
        echo
        echo Verifying $i
        gpg --verify $i $(basename $i .sig)
    done
}

function upload() {
    verify
    chmod ugo+r,o-w -R .
    rsync --progress -aLz $rsync_opts . $host:public_html/$rel_name
    chmod ugo-w $(ls *.xz *.bz2)
    # Purge CDN cache
    curl -X PURGE http://downloads.haskell.org/~ghc/$rel_name/
    for i in *; do
        purge_file $i
    done
}

function purge_file() {
    curl -X PURGE http://downloads.haskell.org/~ghc/$rel_name/$i/
    curl -X PURGE http://downloads.haskell.org/~ghc/$rel_name/$i/docs/
}

function prepare_docs() {
    rm -Rf docs
    mkdocs="$ghc_tree/distrib/mkDocs/mkDocs"
    if [ ! -e $mkdocs ]; then
        echo "Couldn't find GHC mkDocs at $mkdocs."
        echo "Perhaps you need to override ghc_tree?"
        exit 1
    fi
    windows_bindist="$(ls ghc-$ver-x86_64-unknown-mingw32.tar.xz | head -n1)"
    linux_bindist="$(ls ghc-$ver-x86_64-deb8-linux.tar.xz | head -n1)"
    echo "Windows bindist: $windows_bindist"
    echo "Linux bindist: $linux_bindist"
    $ENTER_FHS_ENV $mkdocs $linux_bindist $windows_bindist

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
    sign
    if [ ! -d docs ]; then
        prepare_docs || ( rm -R docs; exit 1 )
    fi
    upload
else
    $@
fi
