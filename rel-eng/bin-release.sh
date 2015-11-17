#!/bin/bash -e

set +o pipefail

args="$@"

function log() {
    echo "bin-release: $@"
    echo "$@" >> $log
}

function usage() {
    cat <<-EOF
Usage:
  $0 [action]

where [action] may be one of,
  [nothing]      do an automatic build
  fetch          fetch source tarballs
  prepare        prepare host environment for build
  do_build       build the binary distribution
  testsuite      run the testsuite on the built tree
  test_install   test the binary distribution
  upload         upload to staging directory

Relevant environment variables:
  NTHREADS       Number of CPUs to use
  CONFIGURE_OPTS Other options to pass to `configure`
                 On Windows these will be helpful
                     64-bit:   --host=x86_64-w64-mingw32
                     32-bit:   --host=i686-w64-mingw32

Example:
  export ver="7.10.2.20151105"
  export rel_name="7.10.2-rc2"
  NTHREADS=4 $0

EOF
}

function fetch() {
    if [ -z "$rel_name" ]; then
        echo "Please set rel_name environment variable (e.g. 7.10.3-rc2)"
    fi
    if [ -z "$download_url" ]; then
        download_url="http://home.smart-cactus.org/~ben/ghc/release-prep"
    fi
    log "fetching tarballs from $download_url"
    wget -N $download_url/$rel_name/ghc-$ver-src.tar.bz2
    wget -N $download_url/$rel_name/ghc-$ver-testsuite.tar.bz2
}

function setup_debian() {
    sudo apt-get install libgmp10-dev dblatex docbook-xsl python-sphinx
}

function setup_redhat() {
    cat centos-6.6.pkgs | sudo xargs yum install -y
}

function setup_windows() {
    log "Running Windows... Good luck."
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
            log "Unknown distribution"
        fi
    fi

    if [ ! -e bin/hscolour ]; then
        log "installing hscolour"
        cabal install --reinstall --bindir=$bin_dir hscolour
    fi

    if [ -d ghc-$ver ]; then
        log "Using existing tree"
    else
        tar -jxf ghc-$ver-src.tar.bz2
        tar -jxf ghc-$ver-testsuite.tar.bz2

        # In the case of rc tarballs the source directory name may not match $ver
        root_dir="$(basename $(tar -jtf ghc-$ver-src.tar.bz2 | head -n1))"
        if [ "$root_dir" != "ghc-$ver" ]; then
            mv $root_dir ghc-$ver
        fi
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
        log "dblatex not available"
        # dblatex is unavailable on CentOS yet GHC is quite bad at realizing this
        cat >> mk/build.mk <<EOF
BUILD_DOCBOOK_PDF=YES
EOF
    fi

    log "Bootstrap GHC at $(which ghc)"
    log "Bootstrap GHC says $(ghc -V)"
    log "configuring with $configure_opts"
    ./configure $configure_opts 2>&1 | tee $root/conf.log
    make -j$NTHREADS 2>&1 | tee $root/make.log
    make binary-dist 2>&1 | tee $root/binary-dist.log
    make test_bindist 2>&1 | tee $root/test-bindist.log
    cd ..
    log "binary dist build finished"
}

function testsuite() {
    log "running testsuite"
    make test NTHREADS=$NTHREADS 2>&1 | tee $root/testsuite.log
}

function test_install() {
    rm -Rf test
    mkdir test
    tar -jx -C test -f $root/ghc-$ver/ghc-$ver-*.tar.bz2
    cd test/ghc-$ver
    log "configuring test rebuild"
    test_root=$(pwd)/../inst
    ./configure --prefix=$test_root $configure_opts | tee ../test-rebuild
    log "installing test rebuild"
    make install

    cat > ../hi.hs <<-EOF
main = print "hello world!"
EOF
    $test_root/bin/ghc ../hi.hs
    ../hi

    log "test rebuild successful; things look good."
}

function upload() {
    upload_dir="ben@home.smart-cactus.org:public_html/ghc/release-prep/$rel_name"
    log "Uploading to $upload_dir"
    scp $root/ghc-$ver/ghc-$ver-*.tar.bz2 $upload_dir
}

if [ -z "$ver" ]; then
   usage
   exit 1
fi

root="$(pwd)/bin-dist-$ver"
bin_dir="$root/bin"
mkdir -p $root $root/bin
PATH="$bin_dir:$PATH"

log="$root/log"
echo >> $log
log "invoked with: $args"

cd bin-dist-$ver

if [ $# == 0 ]; then
    fetch
    prepare
    do_build
    test_install
else
    $1
fi
