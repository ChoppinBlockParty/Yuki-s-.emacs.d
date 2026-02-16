# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

Personal Emacs configuration built around Evil mode (Vim emulation). Emacs 28.2 compiled with `--with-json` and `--with-native-compilation`.

## Architecture

**Entry point:** `init.el` — sets up GC tuning, bootstraps `use-package` from MELPA, then loads modular config files via `require`.

**Config modules:** All in `configs/` directory, loaded in a specific order defined in `init.el`. Each file is a `provide`-based module (e.g., `(provide 'evil-config)`). The load order matters — `base-config` loads first, `global-key-binding-config` loads last.

**Local packages:** `local/` contains vendored/forked packages (evil, avy, emacs-ycmd, protobuf) loaded via `:load-path` in `use-package` declarations.

**Package management:** `use-package` with MELPA/GNU/Org ELPA archives. `use-package-always-ensure` is `t`, so packages auto-install. Installed packages go to `elpa/`.

**Build scripts:** `tools/` contains shell scripts for building Emacs from source on Ubuntu:
```shell
sudo ./tools/install-prerequisits.sh
sudo ./tools/install-emacs.sh /opt/emacs
```

## Conventions

- **Custom function prefix:** All custom functions use `my-` prefix (e.g., `my-buffer-formatting`, `my-evil-all-modes-define-key`)
- **Syntax tables:** Underscore and hyphen are consistently made word characters via `(modify-syntax-entry ?_ "w")` and `(modify-syntax-entry ?- "w")` across modes
- **Cache/state paths:** Everything stored under `~/.cache/emacs/` (backups, autosave, saveplace, savehist, recentf, projectile-cache)
- **Custom-set-variables:** Embedded at the bottom of `init.el` (no separate `custom.el`)
- **Keybinding priority:** `my-intercept-mode-map` is a custom minor mode map with `emulation-mode-map-alists` priority, used for bindings that must override all other modes
- **Comment all changes:** Every new configuration line or snippet must have an accompanying comment explaining its purpose.

## Key Modules (by importance)

| File | Purpose |
|------|---------|
| `base-config.el` | Core settings: UI, fonts, scrolling, encoding, large file handling, prog-mode hooks |
| `evil-config.el` | Evil mode setup, custom motions, SPC-leader bindings, visual-star search |
| `evil-collection.el` | Evil keybindings for non-editing modes (magit, dired, etc.) |
| `completion-config.el` | LSP-mode (C/C++, Python/pyright), company-mode with TNG frontend |
| `ivy-config.el` | Ivy/Counsel/Swiper as primary completion framework |
| `magit-config.el` | Git integration with extensive Evil keybindings |
| `dired-config.el` | File manager with custom Evil bindings and directory navigation |
| `projectile-config.el` | Project management with Ivy integration, alien indexing |
| `vterm-config.el` | Terminal emulator with command replay utilities |
| `global-key-binding-config.el` | Final keybinding overrides (loaded last) |

## Keybinding Patterns

- **SPC prefix (leader):** `SPC h/j/k/l` window nav, `SPC a` find-other-file, `SPC d` lsp-find-type-definition, `SPC s` format buffer, `SPC w` kill buffer
- **M- prefix (Meta/Cmd):** `M-w` switch-buffer, `M-e` fzf, `M-f` find-file, `M-a` projectile-rg, `M-s` swiper, `M-d` counsel-projectile
- **Window management:** `\` vertical split, `-` horizontal split, `q` close window
- **DEL (backspace):** Bound to swiper in normal mode (with visual selection support)

## Platform

macOS primary: `mac-command-key-is-meta t`, font "DejaVuSansM Nerd Font Mono-16", frame 127x47. Linux fallback font: "DejaVu Sans Mono-13".
