#!/usr/bin/env bash

set -o pipefail
set -e

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
  do_configure   configure source tree
  do_build       build the binary distribution
  testsuite      run the testsuite on the built tree
  test_install   test the binary distribution
  upload         upload to staging directory

Relevant environment variables:
  ver            the version of the source release
                 (e.g. 7.10.2.20151105 or 7.10.2)
  rel_name       the official name of the release used to identify its
                 download directory (e.g. 7.10.2, 7.10.2, or 7.10.3-rc2)
  NTHREADS       number of CPUs to use
  configure_opts other options to pass to \`configure\`
                 on Windows these will be helpful
                     64-bit:   --host=x86_64-w64-mingw32
                     32-bit:   --host=i686-w64-mingw32
  dwarf=1        enable DWARF support and debug information

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
    wget -N $download_url/$rel_name/ghc-$ver-src.tar.xz
    wget -N $download_url/$rel_name/ghc-$ver-testsuite.tar.xz
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

function setup_darwin() {
    for i in automake autoconf gcc docbook docbook-xsl docbook2x psutils; do
        brew install $i
    done
}

function prepare() {
    if [ ! -z "$skip_pkgs" ]; then
        if ! grep CentOS /etc/issue; then
            opsys=linux
            setup_redhat
        elif ! grep Debian /etc/issue; then
            opsys=linux
            setup_debian
        elif ! grep Ubuntu /etc/issue; then
            opsys=linux
            setup_debian
        elif test "$OS" = "Windows_NT"; then
            opsys=windows
            setup_windows
        elif test "`uname`" = "Darwin"; then
            opsys=darwin
            setup_darwin
        elif test "`uname`" = "FreeBSD"; then
            opsys=freebsd
        elif test "`uname`" = "OpenBSD"; then
            opsys=openbsd
        else
            log "Unknown distribution"
        fi
    fi

    if [ ! -e $bin_dir/hscolour ]; then
        log "installing hscolour"
        cabal install --reinstall --bindir=$bin_dir hscolour
    fi

    if [ -d ghc ]; then
        log "Using existing tree"
    else
        $tar -Jxf ghc-$ver-src.tar.xz
        $tar -Jxf ghc-$ver-testsuite.tar.xz

        root_dir="$(basename $($tar -Jtf ghc-$ver-src.tar.xz | head -n1))"
        mv $root_dir ghc
    fi
}

setup_env() {
    if [ -z "$NTHREADS" ]; then NTHREADS=1; fi
    if [ -z "$tar" ]; then tar=tar; fi
    if [ -z "$make" ]; then make=make; fi

    PATH="$bin_dir:$PATH"
    case $(uname) in
        MINGW*)
            configure_opts="$configure_opts --enable-tarballs-autodownload"
            ;;
        Darwin)
            export MACOSX_DEPLOYMENT_TARGET=10.7
            log "MACOSX_DEPLOYMENT_TARGET = $MACOSX_DEPLOYMENT_TARGET"
            # Only Sierra supports clock_gettime. See #12858.
            log "Disabling clock_gettime"
            export ac_cv_func_clock_gettime=no
            configure_opts="$configure_opts CC=/usr/local/bin/gcc-7"
            log "Using Homebrew's gcc $(gcc -dumpversion)"
            ;;
        FreeBSD)
            if uname -a | grep "10.3-RELEASE"; then
                # The gold version in FreeBSD 10.3 is affected by
                # https://sourceware.org/bugzilla/show_bug.cgi?id=12771
                configure_opts="$configure_opts --disable-ld-override"
            fi
            log "Disabling large address space support."
            configure_opts="$configure_opts --disable-large-address-space"
            make=gmake
            tar=gtar
            ;;
        OpenBSD)
            configure_opts="$configure_opts \
                            --with-iconv-includes=/usr/local/include --with-iconv-libraries=/usr/local/lib \
                            --with-gmp-libraries=/usr/local/lib --with-gmp-includes=/usr/local/include \
                            --with-system-libffi --with-ffi-includes=/usr/local/include --with-ffi-libraries=/usr/local/lib"
            make=gmake
            tar=gtar
            export AUTOCONF_VERSION=2.69
            export AUTOMAKE_VERSION=1.15
            ;;
        DragonFly)
            log "Disabling large address space support."
            configure_opts="$configure_opts --disable-large-address-space \
                            --with-curses-includes=/usr/local/include --with-curses-libraries=/usr/local/lib"
            make=gmake
            ;;
    esac
}

function do_configure() {
    cd ghc
    cat > mk/build.mk <<EOF
V=1
HADDOCK_DOCS=YES
LATEX_DOCS=YES
HSCOLOUR_SRCS=YES
BUILD_DOCBOOK_HTML=YES
BeConservative=YES
EOF

    # DWARF support
    if [ -n "$dwarf" ]; then
        log "enabling DWARF support"
        echo "GhcLibHcOpts = -g3" >> mk/build.mk
        echo "GhcRtsHcOpts = -g3" >> mk/build.mk
        configure_opts="$configure_opts --enable-dwarf-unwind"
    fi

    case $(uname) in
        Darwin)
            log "using in-tree GMP"
            echo 'libraries/integer-gmp_CONFIGURE_OPTS += --configure-option=--with-intree-gmp' >> mk/build.mk
            ;;
    esac

    if which dblatex; then
        echo 'BUILD_DOCBOOK_PDF=YES' >> mk/build.mk
        log "dblatex is available"
    else
        # dblatex is unavailable on CentOS yet GHC is quite bad at realizing this
        echo 'BUILD_DOCBOOK_PDF=NO' >> mk/build.mk
        log "dblatex not available"
    fi

    log "Bootstrap GHC at $(which ghc)"
    log "Bootstrap GHC says $(ghc -V)"
    log "configuring with $configure_opts"
    ./configure $configure_opts 2>&1 | tee $root/configure.log
    cd ..
    log "binary dist configure finished"
}

function do_build() {
    cd ghc
    $make -j$NTHREADS 2>&1 | tee $root/make.log
    $make binary-dist 2>&1 | tee $root/binary-dist.log
    $make test_bindist 2>&1 | tee $root/test-bindist.log
    cd ..
    log "binary dist build finished"
}

function testsuite() {
    cd ghc
    log "running testsuite"
    $make test THREADS=$NTHREADS 2>&1 | tee $root/testsuite.log
}

function test_install() {
    if uname | grep -q MINGW; then
        log "No installation test on Windows"
        return;
    fi
    rm -Rf $root/test
    mkdir $root/test
    $tar -C test -Jxf $root/ghc/ghc-*.tar.xz
    cd $root/test/ghc*
    log "configuring test rebuild"
    test_root=$root/test/inst
    ./configure --prefix=$test_root $configure_opts | tee $root/test-rebuild.log
    log "installing test rebuild"
    $make install | tee $root/test-install.log

    cat > $root/test/hi.hs <<-EOF
main = print "hello world!"
EOF
    $test_root/bin/ghc $root/test/hi.hs
    $root/test/hi

    log "test rebuild successful; things look good."
}

function upload() {
    upload_dir="ben@home.smart-cactus.org:public_html/ghc/release-prep/$rel_name"
    eval $($make -C $root/ghc show VALUE=ProjectVersion | grep ^ProjectVersion)
    tarball="$(ls $root/ghc/ghc-*.tar.xz)"
    dest_tarball="$upload_dir/$(basename $tarball | sed "s/$ProjectVersion/$ver/")"
    log "Uploading to $dest_tarball"
    scp $tarball $dest_tarball
}

if [ -z "$ver" ]; then
   usage
   exit 1
fi

root="$(pwd)/bin-dist-$ver-$(uname)"
bin_dir="$root/bin"
mkdir -p $root $root/bin
PATH="$bin_dir:$PATH"

log="$root/log"
echo >> $log
log "invoked with: $args"
setup_env

cd $root

if [ $# == 0 ]; then
    fetch
    prepare
    do_configure
    do_build
    test_install
else
    $1
fi
