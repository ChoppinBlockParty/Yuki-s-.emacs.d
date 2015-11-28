(add-to-list 'load-path (concat user-emacs-directory "configs"))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("SC"  . "http://joseito.republika.pl/sunrise-commander/")))

(require 'package)
(package-initialize)
(setq package-enable-at-startup nil)
(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

(require 'use-package)

; (require 'crosshairs)

; (use-package column-marker
;   :ensure t
;   :demand
;   :config
;   (progn
;   )
; )

(global-hl-line-mode 1)

;; To customize the background color
(set-face-background 'hl-line "#330")  ;; Emacs 22 Only

(use-package highlight-symbol
  :ensure t
  :config
  (progn
    (global-set-key [(f2)] 'highlight-symbol)
    (global-set-key [f3] 'highlight-symbol-next)
    (global-set-key [(shift f3)] 'highlight-symbol-prev)
    (global-set-key [(meta f3)] 'highlight-symbol-query-replace)
  )
)

(defvar dotspacemacs-editing-style 'vim
  "Either `vim' or `emacs'. Evil is always enabled but if the variable
    is `emacs' then the `holy-mode' is enabled at startup.")

(defvar dotspacemacs-leader-key "SPC"
  "The leader key.")

(require 'elscreen-config)
(require 'core-funcs)
(require 'configs-base)
(require 'core-micro-state)
(require 'config-color)
(require 'config-cl-lib)
(require 'configs-evil)
(require 'configs-dired)
; (require 'golden-ratio-config)
(require 'window-numbering-config)
(require 'auto-fill-mode-config)
(require 'ag-config)
(require 'configs-helm)
; (require 'config-sunrise-commander)
; (require 'ranger-config)
(require 'config-easymotion)
;; (require 'ido-config)
(require 'cua-config)
(require 'spell-check-config)
;; (require 'latex-config)


(require 'config-autocompletion)

(provide 'clojure-mode-config)
(provide 'js2-mode-config)
(require 'markdown-mode-config)

(require 'show-paren-mode-config)
(require 'config-paren)

(require 'icicles-config)
(require 'visual-regexp-config)

(require 'powerline-config)
(require 'moe-theme-config)
(require 'rainbow-mode-config)
(require 'config-rainbow-delimiters)
;; (require 'tab-bar-mode-config)

;; (require 'undo-tree-config)
(require 'projectile-config)
(require 'magit-config)

(require 'config-cider)
(require 'jedi-config)

