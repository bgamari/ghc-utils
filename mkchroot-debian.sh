#!/bin/bash -e

# setup a debootstrap-based debian chroot

function usage() {
    echo "Usage: ARCH=[i386|amd64] $0 [dest]"
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

dest=deb7-$arch
real_dest=$(realpath $dest)
user=$(id -un)

mkdir $dest
sudo debootstrap --arch=$arch wheezy $dest http://http.debian.net/debian/
sudo chown $user $dest

cat >$dest/activate <<EOF
#!/bin/bash
MY_CHROOT=$real_dest
sudo systemd-nspawn --personality=$personality -D \$MY_CHROOT \$@
EOF
chmod ugo+rx $dest/activate

$dest/activate adduser --uid=`id -u` $user
$dest/activate bash -e <<EOF
apt-get update
apt-get install -y build-essential vim bash-completion bash locales autoconf libncurses-dev git python sudo curl console-data locales-all libgmp-dev cabal-install zlib1g-dev

sed -i '/^%sudo/d' /etc/sudoers
echo "%sudo ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers
adduser $user sudo

mkdir -p /opt/ghc
chown $user /opt/ghc
EOF

# When building binary releases
#$dest/activate apt-get install dblatex docbook-xsl

$dest/activate sudo -u $user -- bash -e <<EOF
cd
git clone https://bgamari@github.com/bgamari/ghc-utils.git
cd ghc-utils
./setup-chroot
EOF
