#!/bin/bash -e

# See https://zaufi.github.io/administration/2014/06/10/howto-make-a-centos-chroot/
#sudo apt-get install yum centos

if [ -z "$1" ]; then
    echo "Usage: $0 [dest]"
    exit 1
fi

dest=$(realpath $1)
user=$(id -un)

mkdir $dest $dest/tmp
rpm --root=$dest --rebuilddb
sudo chown $user $dest
sudo chmod u+rwx $dest

curl http://mirror.centos.org/centos/6/os/x86_64/Packages/centos-release-6-6.el6.centos.12.2.x86_64.rpm > $dest/tmp/centos-release.rpm
sudo rpm --root=$dest --nodeps --install $dest/tmp/centos-release.rpm
sudo sed -i -e 's/gpgcheck=1/gpgcheck=0/' $dest/etc/yum.repos.d/CentOS-Base.repo

sudo yum --installroot=$dest update
sudo yum --installroot=$dest -y install yum

cat >$dest/activate <<EOF
#!/bin/bash
MY_CHROOT=$dest

sudo mount proc \$MY_CHROOT/proc -t proc
sudo mount sysfs \$MY_CHROOT/sys -t sysfs
sudo cp /etc/hosts \$MY_CHROOT/etc/hosts
sudo cp /proc/mounts \$MY_CHROOT/etc/mtab
sudo chroot \$MY_CHROOT \$@
EOF
chmod ugo+rx $dest/activate

cat >$dest/stop <<EOF
#!/bin/bash
MY_CHROOT=$dest

sudo umount \$MY_CHROOT/proc
sudo umount \$MY_CHROOT/sys
EOF
chmod ugo+rx $dest/stop

# RPM installed by Debian may not be the same version wanted by CentOS
# Rebuild the package database with the CentOS RPM
$dest/activate  <<EOF
rm -rf /var/lib/rpm
rpm --rebuilddb
rpm --nodeps -i /tmp/centos-release.rpm
yum install -y yum file git sudo

adduser $user sudo
mkdir -p /opt/ghc
chown $user /opt/ghc
EOF
rm $dest/tmp/centos-release.rpm

$DEST/activate sudo -u $user -- bash -e <<EOF
cd
git clone https://bgamari@github.com/bgamari/ghc-utils.git
cd ghc-utils
./setup-chroot
EOF
