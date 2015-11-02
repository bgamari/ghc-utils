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
    echo
    echo "Other environment variables:"
    echo "  NTHREADS"
    exit 1
fi

mkdir -p bin-dist-$ver

cd bin-dist-$ver

function setup_debian() {
    sudo apt-get install dblatex docbook-xsl python-sphinx
}

function setup_redhat() {
    cat centos-6.6.pkgs | sudo xargs yum install -y
}

function setup_windows() {
    echo "Running Windows... Good luck."
    pacman -Sy pacman -S mingw-w64-$(uname -m)-python2-sphinx
}

function prepare() {
    if [ ! -z "$skip_pkgs" ]; then
        if ! grep CentOS /etc/issue; then
            setup_redhat
        elif ! grep Debian /etc/issue; then
            setup_debian
        elif ! grep Ubuntu /etc/issue; then
            setup_debian
        elif test "$OS" = "Windows_NT"; then
            setup_windows
        else
            echo "Unknown distribution"
        fi
    fi

    cabal install hscolour
    rm -Rf ghc-$ver
    tar -jxf ../ghc-$ver-src.tar.bz2
    tar -jxf ../ghc-$ver-testsuite.tar.bz2

    # In the case of rc tarballs the source directory name may not match $ver
    root_dir="$(basename $(tar -jtf ../ghc-$ver-src.tar.bz2 | head -n1))"
    if [ "$root_dir" != "$ver" ]; then
       ln -s $root_dir ghc-$ver
    fi
}

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
EOF
    fi

    ./configure      2>&1 | tee ../conf.log
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
    ./configure --prefix=$(realpath ..)/inst
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
