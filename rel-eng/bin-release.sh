#!/bin/bash -e

# Usage:
#
#   mkdir build; cd build
#   export ver="7.10.2"
#   wget http://downloads.haskell.org/~ghc/$ver/ghc-$ver-src.tar.bz2
#   wget http://downloads.haskell.org/~ghc/$ver/ghc-$ver-testsuite.tar.bz2
#   bash bin-release.sh

mkdir -p bin-dist-$ver

cd bin-dist-$ver
#rm -R ghc-$ver

function setup_debian() {
    sudo apt-get install dblatex docbook-xsl
}

function setup_redhat() {
    cat centos-6.6.pkgs | sudo xargs yum install -y
}

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


cabal install hscolour

tar -jxf ../ghc-$ver-src.tar.bz2
tar -jxf ../ghc-$ver-testsuite.tar.bz2
cd ghc-$ver

cat > mk/build.mk <<EOF
V=1
HADDOCK_DOCS=YES
LATEX_DOCS=YES
HSCOLOUR_SRCS=YES
BUILD_DOCBOOK_HTML=YES
BUILD_DOCBOOK_PDF=YES
BUILD_DOCBOOK_PS=YES
BeConservative=YES
EOF

./configure      2>&1 | tee ../conf.log
make             2>&1 | tee ../make.log
make binary-dist 2>&1 | tee ../binary-dist.log

cd ..
rm -R test
mkdir test
tar -jx -C test -f ../ghc-$ver/ghc-$ver-$thing.tar.bz2
cd ghc-$ver
./configure --prefix=$(realpath ..)/inst
make
make install

echo "Things look good."
