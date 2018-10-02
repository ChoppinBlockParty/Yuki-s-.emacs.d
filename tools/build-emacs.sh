#! /usr/bin/env bash

set -e

### -flto required ld.gold, otherwise results in segmentation faults
## Set ld.gold as default
#sudo rm /usr/bin/ld
#sudo ln -s /usr/bin/x86_64-linux-gnu-ld.gold  /usr/bin/ld
## Restore ld.bfd
#sudo rm /usr/bin/ld
#sudo ln -s /usr/bin/x86_64-linux-gnu-ld.bfd  /usr/bin/ld


if [[ $# -ne 1 ]];then echo "build-emacs.sh <install prefix>"; exit 1; fi

PREFIX="$1"

export CC=clang-7
export CXX=clang++-7
export AR=llvm-ar-7
export RANLIB=llvm-ranlib-7
export CFLAGS='-O3 -fomit-frame-pointer -fstrict-aliasing -flto -pthread'
export CXXFLAGS='-O3 -fomit-frame-pointer -fstrict-aliasing -flto -pthread'
export LDFLAGS='-flto -pthread'

### So far it is troublesome to compile with static imagemagick
### Causing segmentation faults during PDF open

# IMAGEIMAGICK_BUILD_DIR="`pwd`/.imagemagick-build"
# IMAGEIMAGICK_INSTALL_DIR="`pwd`/.imagemagick-install"

# pushd . > /dev/null

# if [[ ! -d $IMAGEIMAGICK_BUILD_DIR ]]; then
#   mkdir "$IMAGEIMAGICK_BUILD_DIR"
#   cd "$IMAGEIMAGICK_BUILD_DIR"
#   git clone https://github.com/ImageMagick/ImageMagick6.git
# fi

# if [[ ! -d $IMAGEIMAGICK_INSTALL_DIR ]]; then
#   cd "$IMAGEIMAGICK_BUILD_DIR/ImageMagick6"
#   make clean || true
#   git clean -fd
#   git fetch origin
#   # https://github.com/ImageMagick/ImageMagick6/releases
#   git c 6.9.10-12
#   ./configure \
#     --prefix="$IMAGEIMAGICK_INSTALL_DIR" \
#     --enable-gold \
#     --enable-static=yes \
#     --enable-shared=no
#   make V=1
#   make install
# fi

# popd > /dev/null

# CFLAGS="-I$IMAGEIMAGICK_INSTALL_DIR/include $CFLAGS"
# CXXFLAGS="-I$IMAGEIMAGICK_INSTALL_DIR/include $CXXFLAGS"
# LDFLAGS="-L$IMAGEIMAGICK_INSTALL_DIR/lib $LDFLAGS"
# export LD_LIBRARY_PATH="$IMAGEIMAGICK_INSTALL_DIR/lib"
# export PKG_CONFIG_PATH="$IMAGEIMAGICK_INSTALL_DIR/lib/pkgconfig"
# ### https://stackoverflow.com/questions/21027388/how-to-make-pkg-check-modules-work-with-static-libraries
# export PKG_CONFIG="pkg-config --static"

make clean || true
git pull origin
./autogen.sh
./configure \
  --prefix="$PREFIX" \
  --enable-link-time-optimization \
  --with-x-toolkit=lucid

make V=1
sudo make install
