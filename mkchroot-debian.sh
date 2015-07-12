#!/bin/bash -e

# setup a debootstrap-based debian chroot

ARCH=i386
#ARCH=amd64
dest=deb7-$ARCH
real_dest=$(realpath $dest)
user=$(id -un)

mkdir $dest $dest/dev $dest/dev/pts
sudo debootstrap --arch=$ARCH wheezy $dest http://http.debian.net/debian/
sudo chown $user $dest

cat >$dest/activate <<EOF
#!/bin/bash
MY_CHROOT=$real_dest
sudo systemd-nspawn -D \$MY_CHROOT \$@
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
