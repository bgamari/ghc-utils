#!/bin/bash -e

# setup a debootstrap-based debian chroot

#ARCH=i386
ARCH=amd64
DEST=deb7-$ARCH
REAL_DEST=$(realpath $DEST)
user=$(id -un)

mkdir $DEST $DEST/dev $DEST/dev/pts
sudo debootstrap --arch=$ARCH wheezy $DEST http://http.debian.net/debian/
sudo chown $user $DEST

cat >$DEST/activate <<EOF
#!/bin/bash
MY_CHROOT=$REAL_DEST

sudo mount proc \$MY_CHROOT/proc -t proc
sudo mount sysfs \$MY_CHROOT/sys -t sysfs
sudo mount devpts \$MY_CHROOT/dev/pts -t devpts
sudo cp /etc/hosts \$MY_CHROOT/etc/hosts
sudo cp /proc/mounts \$MY_CHROOT/etc/mtab
sudo chroot \$MY_CHROOT \$@
EOF
chmod ugo+rx $DEST/activate

cat >$DEST/stop <<EOF
#!/bin/bash
MY_CHROOT=$REAL_DEST

sudo umount \$MY_CHROOT/proc
sudo umount \$MY_CHROOT/sys
sudo umount \$MY_CHROOT/dev/pts
EOF
chmod ugo+rx $DEST/stop

$DEST/activate adduser --uid=`id -u` $user
$DEST/activate bash -e <<EOF
apt-get update
apt-get install -y build-essential vim bash-completion bash locales autoconf libncurses-dev git python sudo curl console-data locales-all libgmp-dev cabal-install zlib1g-dev

sed -i '/^%sudo/d' /etc/sudoers
echo "%sudo ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers
adduser $user sudo

mkdir -p /opt/ghc
chown $user /opt/ghc
EOF

# When building binary releases
#$DEST/activate apt-get install dblatex docbook-xsl

$DEST/activate sudo -u $user -- bash -e <<EOF
cd
git clone https://bgamari@github.com/bgamari/ghc-utils.git
cd ghc-utils
./setup-chroot
EOF
