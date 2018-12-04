#! /usr/bin/env bash

set -e

if [[ $# -ne 1 ]]; then echo "build-emacs.sh <install prefix>"; exit 1; fi

PREFIX="$1"

export CFLAGS='-O3 -fomit-frame-pointer -fstrict-aliasing -pthread'
export CXXFLAGS='-O3 -fomit-frame-pointer -fstrict-aliasing -pthread'
export LDFLAGS='-pthread'

# ## -flto required ld.gold, otherwise results in segmentation faults
# # Set ld.gold as default
# sudo rm /usr/bin/ld; sudo ln -s /usr/bin/x86_64-linux-gnu-ld.gold  /usr/bin/ld
# # Restore ld.bfd
# sudo rm /usr/bin/ld; sudo ln -s /usr/bin/x86_64-linux-gnu-ld.bfd  /usr/bin/ld
if [ -x "$(command -v clang 2>/dev/null)" ]; then
  export CC=clang
  export CXX=clang++
  export AR=llvm-ar
  export RANLIB=llvm-ranlib
  export CFLAGS="$CFLAGS -flto"
  export CXXFLAGS="$CXXFLAGS -flto"
  export LDFLAGS="$LDFLAGS -flto"
fi

function clone_update_git_repo {
  local new_dirpath="`pwd`/$(basename "$1")"
  if [[ -d $new_dirpath ]]; then
    echo "  -- Does not clone \"$1\": \"$new_dirpath\" exists"
  else
    git clone "$1"
    echo "  -- Cloned \"$1\" to \"$new_dirpath\""
  fi
  local branch="${2:-master}"
  cd "$new_dirpath"
  git checkout "$branch"
  # FIXME: if `branch` is a tag, pull fails
  git pull origin || true
  git submodule update --recursive --init
  echo "  -- Update \"$1\", branch $branch"
}

clone_update_git_repo https://github.com/emacs-mirror/emacs emacs-26
make clean || true
./autogen.sh
./configure \
  --prefix="$PREFIX" \
  --enable-link-time-optimization \
  --with-x-toolkit=lucid

make V=1
sudo make install
