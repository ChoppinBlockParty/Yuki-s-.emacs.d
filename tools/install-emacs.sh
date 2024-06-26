#! /usr/bin/env bash

set -e

if [[ $# -ne 1 ]]; then echo "build-emacs.sh <install prefix>"; exit 1; fi

SCRIPT_DIR="$(realpath "$(dirname "$0")")"

PREFIX="$1"

function clone_update_git_repo {
  local new_dirpath="`pwd`/$(basename "$1")"
  if [[ -d $new_dirpath ]]; then
    echo "  -- Does not clone \"$1\": \"$new_dirpath\" exists"
  else
    git clone "$1"
    echo "  -- Cloned \"$1\" to \"$new_dirpath\""
  fi
  cd "$new_dirpath"
  git pull origin || true
  local branch="${2:-master}"
  git checkout "$branch"
  git submodule update --recursive --init
  echo "  -- Update \"$1\", branch $branch"
}

clone_update_git_repo https://github.com/emacs-mirror/emacs emacs-29.2
make clean || true
./autogen.sh
# On Linux
#  --with-x-toolkit=lucid \
./configure \
  --prefix="$PREFIX" \
  --enable-link-time-optimization \
  --with-json \
  --with-native-compilation

make V=1
sudo make install
mkdir -p ~/bin
ln -fs "$PREFIX/bin/ctags" ~/bin
ln -fs "$PREFIX/bin/ebrowse" ~/bin
ln -fs "$PREFIX/bin/emacs" ~/bin/emacs
ln -fs "$PREFIX/bin/emacsclient" ~/bin
ln -fs "$PREFIX/bin/etags" ~/bin

cp "$SCRIPT_DIR/emacs.desktop" ~/.local/share/applications

if [[ `uname` != 'Darwin' ]]; then
    update-desktop-database ~/.local/share/applications
fi
