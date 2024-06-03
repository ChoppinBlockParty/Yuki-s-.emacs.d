#! /usr/bin/env bash

set -e

version=${1:-29.2}

if [[ $# -ne 1 ]]; then echo "build-emacs.sh <version>"; exit 1; fi

SCRIPT_DIR="$(realpath "$(dirname "$0")")"

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

clone_update_git_repo https://github.com/emacs-mirror/emacs emacs-${version}
make clean || true
./autogen.sh
# On Linux
#  --with-x-toolkit=lucid \
#
./configure \
  --enable-link-time-optimization \
  --with-json \
  --with-native-compilation

make V=1
# Does not really isntalls though prepares everything for the move below.
make install
if [ -d /Applications/Emacs.app ]; then
    mv /Application/Emacs.app /opt/emacs-old
fi
mv "${SCRIPT_DIR}/emacs/nextstep/Emacs.app" /Applications
