#! /usr/bin/env bash

set -e

# ## -flto required ld.gold, otherwise results in segmentation faults
# # Set ld.gold as default
# sudo rm /usr/bin/ld; sudo ln -s /usr/bin/x86_64-linux-gnu-ld.gold  /usr/bin/ld
# # Restore ld.bfd
# sudo rm /usr/bin/ld; sudo ln -s /usr/bin/x86_64-linux-gnu-ld.bfd  /usr/bin/ld


if [[ $# -ne 1 ]];then echo "build-emacs.sh <install prefix>"; exit 1; fi

PREFIX="$1"

export CC=clang
export CXX=clang++
export AR=llvm-ar
export RANLIB=llvm-ranlib
export CFLAGS='-O3 -fomit-frame-pointer -fstrict-aliasing -pthread'
export CXXFLAGS='-O3 -fomit-frame-pointer -fstrict-aliasing -pthread'
export LDFLAGS='-pthread'

make clean || true
git pull origin
./autogen.sh
./configure \
  --prefix="$PREFIX" \
  --enable-link-time-optimization \
  --with-x-toolkit=lucid

make V=1
sudo make install
