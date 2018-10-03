#! /usr/bin/env bash

set -e

# ## -flto required ld.gold, otherwise results in segmentation faults
# # Set ld.gold as default
# sudo rm /usr/bin/ld; sudo ln -s /usr/bin/x86_64-linux-gnu-ld.gold  /usr/bin/ld
# # Restore ld.bfd
# sudo rm /usr/bin/ld; sudo ln -s /usr/bin/x86_64-linux-gnu-ld.bfd  /usr/bin/ld


if [[ $# -ne 1 ]];then echo "build-emacs.sh <install prefix>"; exit 1; fi

PREFIX="$1"

export CC=clang-7
export CXX=clang++-7
export AR=llvm-ar-7
export RANLIB=llvm-ranlib-7
export CFLAGS='-O3 -fomit-frame-pointer -fstrict-aliasing -flto -pthread'
export CXXFLAGS='-O3 -fomit-frame-pointer -fstrict-aliasing -flto -pthread'
export LDFLAGS='-flto -pthread'

make clean || true
git pull origin
./autogen.sh
./configure \
  --prefix="$PREFIX" \
  --enable-link-time-optimization \
  --with-x-toolkit=lucid

make V=1
sudo make install
