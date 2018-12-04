#! /usr/bin/env bash

packages=(
  ### Makeinfo
  texinfo

  ### Required for pdf-tools
  zlib1g-dev
  libpoppler-glib-dev
  libpoppler-private-dev

  ### Required for Emacs
  #	X11 pixmap library
  libxpm-dev
  libjpeg-dev
  libtiff-dev
  libgif-dev
  libpng-dev
  librsvg2-dev
  # X11 authorisation library
  libxdmcp-dev
  # Little Color Managment System
  liblcms2-dev
  libsystemd-dev
  libcairo2-dev
  libxml2-dev
  # Improve image manipulion
  libmagickwand-dev
  # FreeType-based font drawing library for X (
  libxft-dev
  libotf-dev
  # Use m17n-flt for text shaping
  libm17n-dev

  ### X11 stuff for Lucid GUI and everything else...
  libx11-dev
  libx11-xcb-dev
  libxcb1-dev
  libxfixes-dev
  libxinerama-dev
  libxmu-dev
  libxrandr-dev
  libxrender-dev
  libxt-dev
  libxaw7-dev
  xaw3dg-dev
  libxau-dev
  libxcursor-dev
  libxcomposite-dev
  libxdamage-dev
  libxkbcommon-dev
  libxi-dev
  libxext-dev

  # General Purpose Mouse
  libgpm-dev
  libpango1.0-dev
  libatk1.0-dev
  libgdk-pixbuf2.0-dev
  libgio3.0-cil-dev
  libglib2.0-dev
  libsm-dev
  libice-dev
  libdbus-1-dev
  libtinfo-dev
  libfreetype6-dev
  libfontconfig-dev
  libz-dev
  libgnutls28-dev
  liblzma-dev
  libjbig-dev
  libepoxy-dev
  libthai-dev
  libpixman-1-dev
  libffi-dev
  libpcre3-dev
  uuid-dev
  libicu-dev
  libexpat1-dev
  libp11-kit-dev
  libidn2-0-dev
  libtasn1-dev
  nettle-dev
  libgmp-dev
  libgcrypt-dev
  libharfbuzz-dev
  libdatrie-dev
  libgpg-error-dev
  libgraphite2-dev
  )
apt-get -y install --no-install-recommends "${packages[@]}"
