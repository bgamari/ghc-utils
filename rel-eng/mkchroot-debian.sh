#!/bin/bash -e

# setup a debootstrap-based debian chroot

function usage() {
    cat <<EOF
Usage: ARCH=[i386|amd64] DIST=[jessie|wheezy] $0 [dest]"

Options:
    ARCH:       Architecture
    DIST:       Distribution name
    SCRIPT:     debootstrap script name
    deb_repo:   Which .deb repository to fetch packages from
                e.g. http://http.debian.net/debian/ for Debian
                     https://mirrors.kernel.org/ubuntu/ for Ubuntu
                     http://old-releases.ubuntu.com/ubuntu/ for older Ubuntu releases

EOF
}

case $ARCH in
    amd64|x86_64)
        arch=amd64
        personality=x86-64
        ;;
    i386)
        arch=i386
        personality=x86
        ;;
    *)
        echo "Unknown architecture $arch"
        usage
        exit 1
        ;;
esac

dest=$1
real_dest=$(realpath $dest)
user=$(id -un)
dist="$DIST"
if [ -z "$deb_repo" ]; then deb_repo="http://http.debian.net/debian/"; fi
if [ -z "$dist" ]; then dist="jessie"; fi

mkdir $dest
sudo debootstrap --arch=$arch $dist $dest $deb_repo $SCRIPT
sudo chown $user $dest

cat >$dest/activate-root <<EOF
#!/bin/bash
MY_CHROOT=$real_dest
sudo systemd-nspawn --personality=$personality -D \$MY_CHROOT \$@
EOF

cat >$dest/activate <<EOF
#!/bin/bash
$dest/activate-root -u $user -- \$@
EOF

chmod ugo+rx $dest/activate-root $dest/activate

# Debian-only
#other_pkgs="console-data all-locales"

$dest/activate-root adduser --uid=`id -u` $user
$dest/activate-root apt-get update
$dest/activate-root apt-get install -y \
                    build-essential realpath vim bash-completion bash locales autoconf \
                    libncurses-dev git python3 sudo curl \
                    console-data locales-all libgmp-dev zlib1g-dev \
                    $other_pkgs libgmp-dev zlib1g-dev \
                    python3-sphinx

$dest/activate-root bash -e <<EOF
locale-gen en_US.UTF-8
update-locale LANG=en_US.UTF-8

sed -i '/^%sudo/d' /etc/sudoers
echo "%sudo ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers
adduser $user sudo

mkdir -p /opt/ghc
chown $user /opt/ghc
EOF

$dest/activate bash -e <<EOF
cd
git clone https://bgamari@github.com/bgamari/ghc-utils.git
ghc-utils/rel-eng/setup-chroot
EOF
