#!/bin/bash -e

# Usage:
#
#   mkdir build; cd build
#   export ver="7.10.2"
#   wget http://downloads.haskell.org/~ghc/$ver/ghc-$ver-src.tar.bz2
#   wget http://downloads.haskell.org/~ghc/$ver/ghc-$ver-testsuite.tar.bz2
#   NTHREADS=4 bash bin-release.sh

if [ -z "$ver" ]; then
    echo "Usage: ver=7.10.2-rc2 $0"
    exit 1
fi

mkdir -p bin-dist-$ver

cd bin-dist-$ver
mkdir -p bin
bin_dir="$(pwd)/bin"
PATH="$bin_dir:$PATH"

function setup_debian() {
    sudo apt-get install dblatex docbook-xsl
}

function setup_redhat() {
    cat centos-6.6.pkgs | sudo xargs yum install -y
}

function prepare() {
    if [ ! -z "$skip_pkgs" ]; then
        if ! grep CentOS /etc/issue; then
            setup_redhat
        elif ! grep Debian /etc/issue; then
            setup_debian
        elif ! grep Ubuntu /etc/issue; then
            setup_debian
        else
            echo "Unknown distribution"
        fi
    fi

    if [ ! -e bin/hscolour ]; then
        cabal install --reinstall --bindir=$bin_dir hscolour
    fi

    if [ -d ghc-$ver ]; then
        echo "Using existing tree"
        return
    fi

    tar -jxf ../ghc-$ver-src.tar.bz2
    tar -jxf ../ghc-$ver-testsuite.tar.bz2

    # In the case of rc tarballs the source directory name may not match $ver
    root_dir="$(basename $(tar -jtf ../ghc-$ver-src.tar.bz2 | head -n1))"
    if [ "$root_dir" != "$ver" ]; then
        mv $root_dir ghc-$ver
    fi
}

configure_opts="--enable-tarballs-autodownload --with-hscolour=$bin_dir/bin/hscolour $CONFIGURE_OPTS"

function do_build() {
    if [ -z "$NTHREADS" ]; then
        NTHREADS=1
    fi

    cd ghc-$ver
    cat > mk/build.mk <<EOF
V=1
HADDOCK_DOCS=YES
LATEX_DOCS=YES
HSCOLOUR_SRCS=YES
BUILD_DOCBOOK_HTML=YES
BeConservative=YES
EOF
    if ! which dblatex; then
        # dblatex is unavailable on CentOS yet GHC is quite bad at realizing this
        cat >> mk/build.mk <<EOF
BUILD_DOCBOOK_PDF=YES
BUILD_DOCBOOK_PS=YES
EOF
    fi

    ./configure $configure_opts     2>&1 | tee ../conf.log
    make -j$NTHREADS 2>&1 | tee ../make.log
    make binary-dist 2>&1 | tee ../binary-dist.log
    make test_bindist 2>&1 | tee ../test-bindist.log
    cd ..
}

function rebuild() {
    rm -Rf test
    mkdir test
    tar -jx -C test -f ../ghc-$ver/ghc-$ver-*.tar.bz2
    cd test/ghc-$ver
    ./configure --prefix=$(realpath ..)/inst $configure_opts
    make
    make install
    echo "Things look good."
}

if [ $# == 0 ]; then
    prepare
    do_build
    rebuild
else
    $1
fi
