#!/bin/bash -e

# bootstrap an AWS Deban 7 box for GHC development

ghc_ver="7.8.4"

sudo apt-get update
sudo apt-get install -y vim build-essential cabal-install ghc happy mosh tmux git autoconf htop fakeroot curl
sudo apt-get build-dep -y ghc

if false; then
        dist="precise"
        key="F6F88286"

        sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys $key
        sudo tee /etc/apt/sources.list.d/ghc.list > /dev/null <<-EOF
        deb http://ppa.launchpad.net/hvr/ghc/ubuntu $dist main
        deb-src http://ppa.launchpad.net/hvr/ghc/ubuntu $dist main
EOF
        sudo apt-get install ghc-$ghc_ver
else
        sudo mkdir -p /opt/ghc
        sudo chown `whoami` /opt/ghc
        curl -L https://www.haskell.org/ghc/dist/$ghc_ver/ghc-$ghc_ver-x86_64-unknown-linux-deb7.tar.xz | tar -JxC /opt/ghc
        mv /opt/ghc/ghc-$ghc_ver /opt/ghc/$ghc_ver
fi

cat >>$HOME/.bashrc <<EOF
export PATH=/opt/ghc/$ghc_ver/bin:\$PATH
export LD_LIBRARY_PATH=/opt/ghc/$ghc_ver/lib:\$LD_LIBRARY_PATH
EOF
source $HOME/.profile

git clone git://git.haskell.org/ghc
cd ghc
git remote add bgamari http://bgamari@github.com/bgamari/ghc
git submodule update --init
./boot
./configure --prefix=$HOME/root-ghc --with-ghc=/opt/ghc/$ghc_ver/bin/ghc

tmux 

