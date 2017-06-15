#!/bin/bash

set -eux

sudo apt-get -qq update
sudo sh -c "echo 'deb http://llvm.org/apt/precise/ llvm-toolchain-precise-3.7 main' >> /etc/apt/sources.list"
sudo sh -c "echo 'deb http://llvm.org/apt/precise/ llvm-toolchain-precise main' >> /etc/apt/sources.list"
wget -O - http://llvm.org/apt/llvm-snapshot.gpg.key | sudo apt-key add -
sudo add-apt-repository --yes ppa:ubuntu-toolchain-r/test
sudo apt-get -qq update
sudo apt-get install -y libgc-dev clang++-3.7 llvm-3.7 llvm-3.7-dev llvm-3.7-runtime llvm-3.7-tool libunwind7-dev
sudo apt-get install -y make
export CXX=clang++-3.7
git clone https://code.googlesource.com/re2
pushd re2
git checkout 2017-03-01
make -j4 test
sudo make install prefix=/usr
make testinstall prefix=/usr
popd
git clone https://github.com/scala-native/scala-native.git
cd scala-native
../sbt rebuild
cd ..
rm -rf scala-native
