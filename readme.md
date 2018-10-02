Yuki's .emacs.d
===============

After 8 years of Vim, I have finally switch to Emacs. The only regret I have is that I had not done this sooner. Emacs has everything that Vim has and much more.

Build Emacs from source
-----------------------

``` shell
apt-get -y install --no-install-recommends \
# Required for pdf-tools
  zlib1g-dev \
  libpoppler-glib-dev \
  libpoppler-private-dev \
  imagemagick \
# Required for Emacs
  libtiff-dev \
  libjpeg-dev \
  libpng-dev \
  libgif-dev \
  libxpm-dev \
  libgtk-3-dev \
  libpangocairo-dev \
  libpango-dev \
  libatk-dev \
  libcairo-gobject-dev \
  libcairo-dev \
  libgdk_pixbuf-dev \
  libgio-dev \
  libgobject-dev \
  libglib-dev \
  libSM-dev \
  libICE-dev \
  libx11-dev \
  libx11-xcb-dev \
  libxcb-dev \
  libxrender-dev \
  libxft-dev \
  librt-dev \
  libdbus-1-dev \
  libxrandr-dev \
  libxinerama-dev \
  libxfixes-dev \
  libxext-dev \
  libxml2-dev \
  libtinfo-dev \
  libselinux-dev \
  libfreetype-dev \
  libfontconfig-dev \
  libz-dev \
  libotf-dev \
  libgnutls-dev \
  libanl-dev \
  libm-dev \
  libsystemd-dev \
  liblzma-dev \
  libjbig-dev \
  libgmodule-dev \
  libxi-dev \
  libatk-bridge-dev \
  libepoxy-dev \
  libpangoft2-dev \
  libxcursor-dev \
  libxcomposite-dev \
  libxdamage-dev \
  libxkbcommon-dev \
  libwayland-cursor-dev \
  libwayland-egl-dev \
  libwayland-client-dev \
  libmirclient-dev \
  libthai-dev \
  libpixman-1-dev \
  libxcb-shm-dev \
  libxcb-render-dev \
  libresolv-dev \
  libffi-dev \
  libpcre-dev \
  libuuid-dev \
  libdl-dev \
  libxau-dev \
  libxdmcp-dev \
  libicuuc-dev \
  libexpat-de
  libp11-kit-dev \
  libidn-dev \
  libtasn1-dev \
  nettle-dev \
  libhogweed-dev \
  libgmp-dev \
  libgcrypt-dev \
  libatspi-dev \
  libharfbuzz-dev \
  libmircommon-dev \
  libmirprotobuf-dev \
  libcapnp-dev \
  libmircore-dev \
  libboost_system-dev \
  libprotobuf-lite-dev \
  libdatrie-dev \
  libicudata-dev \
  libgpg-error-dev \
  libgraphite2-dev \
  libboost-filesystem-dev
```

Packages
--------

 - [use-package](https://github.com/jwiegley/use-package)
 - [flx](https://github.com/lewang/flx)
 - [smex](https://github.com/nonsequitur/smex)
 - [evil](https://github.com/emacs-evil/evil)
 - [evil-matchit](https://github.com/redguardtoo/evil-matchit)
 - [company-mode](https://github.com/company-mode/company-mode)
 - [emacs-ycmd](https://github.com/abingham/emacs-ycmd)
 - [xterm-color](https://github.com/atomontage/xterm-color)
 - [zerodark-theme](https://github.com/NicolasPetton/zerodark-theme)
 - [powerline](https://github.com/milkypostman/powerline)
 - [rainbow-delimiters](https://github.com/Fanael/rainbow-delimiters)
 - [highlight-symbol.el](https://github.com/nschum/highlight-symbol.el)
 - [evil-easymotion](https://github.com/PythonNut/evil-easymotion)
 - [evil-mc](https://github.com/gabesoft/evil-mc)
 - [projectile](https://github.com/bbatsov/projectile)
 - [emacs-wgrep](https://github.com/mhayashi1120/Emacs-wgrep)
 - [rg.el](https://github.com/dajva/rg.el)
 - [flycheck](https://github.com/flycheck/flycheck)
 - [helm](https://github.com/emacs-helm/helm)
 - [swiper](https://github.com/abo-abo/swiper)
 - [magit](https://github.com/magit/magit)
 - [pdf-tools](https://github.com/politza/pdf-tools)
 - [clang-format](https://github.com/emacsorphanage/clang-format)
 - [lua-mode](https://github.com/immerrr/lua-mode)
 - [go-mode.el](https://github.com/dominikh/go-mode.el)

