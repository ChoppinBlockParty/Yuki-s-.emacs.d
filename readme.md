Yuki's .emacs.d
===============

After 8 years of Vim, I have finally switched to [Emacs](https://www.gnu.org/software/emacs/). The only regret I have is that I had not done this sooner. Emacs has everything that Vim has and much more.

All Vim states (normal, visual, operator, etc.), search (`/`), commands (`:`, including substitution `:s/...`) are already in Emacs, just enable glorious [evil](https://github.com/emacs-evil/evil) package.

Here is a video that gives quite good reasoning why Vim is even better inside Emacs.

[![Why Vim is even better inside Emacs](http://img.youtube.com/vi/JWD1Fpdd4Pc/0.jpg)](http://www.youtube.com/watch?v=JWD1Fpdd4Pc "Why Vim is even better inside Emacs")

Build Emacs from source
-----------------------

Every new [Emacs release](https://www.gnu.org/software/emacs/history.html) is better than the previous one. Here is how I build Emacs from source on Ubuntu (see the scripts inside [tools](tools)).

``` shell
git https://github.com/emacs-mirror/emacs.git
git checkout emacs-26
sudo ./tools/install-prerequisits.sh
sudo ./tools/build-emacs.sh /opt/emacs
ln -s /opt/emacs ~/bin
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

