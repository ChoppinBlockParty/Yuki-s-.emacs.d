;;; init.el -- Emacs startup script

;;; Commentary:
;;; omg

;;; Code:
(add-to-list 'load-path
             (concat user-emacs-directory
                     (convert-standard-filename "configs")))
(add-to-list 'load-path
             (concat user-emacs-directory
                     (convert-standard-filename "local/protobuf")))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

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

(require 'core-funcs)
(require 'configs-base)
(require 'window-numbering-config)
;; (require 'cua-config)
(require 'core-micro-state)
(require 'config-color)
(require 'highlight-config)
(require 'config-cl-lib)
(require 'configs-evil)
(require 'configs-dired)
(require 'auto-fill-mode-config)
(require 'ag-config)
(require 'configs-helm)
(require 'ace-config)
(require 'config-easymotion)
(require 'spell-check-config)
;; (require 'latex-config)


(require 'config-autocompletion)
(require 'config-paren)

(require 'eshell-config)

(provide 'clojure-mode-config)
(provide 'js2-mode-config)
(require 'markdown-mode-config)

(require 'show-paren-mode-config)

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
;; There is ycmd do not jedi
;; (require 'jedi-config)
(require 'vimrc-mode-config)
(require 'docker-config)

(after 'evil
(use-package highlight-symbol
  :ensure t
  :config
  (progn
    (highlight-symbol-mode t)
    (global-set-key [(f2)] 'highlight-symbol)
    (global-set-key [f3] 'highlight-symbol-next)
    (global-set-key [(shift f3)] 'highlight-symbol-prev)
    (global-set-key [(meta f3)] 'highlight-symbol-query-replace)
  )
  )

 (defun my-formatting ()
   ""
   (interactive)
   (when (equal major-mode 'go-mode) (gofmt) )
   )

  (require 'protobuf-mode)
  (add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))

(use-package cmake-mode
  :ensure t
  :config
  (progn
  )
  )

(use-package go-mode
  :ensure t
  :config
  (progn
    (add-to-list `auto-mode-alist '("\\.go\\'" . go-mode))
    (define-key evil-normal-state-map (kbd "SPC s") 'my-formatting)
  )
  )
)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ace-jump-buffer window-numbering visual-regexp vimrc-mode use-package rainbow-mode rainbow-delimiters powerline moe-theme markdown-mode magit highlight-symbol highlight-operators helm-themes helm-swoop helm-projectile helm-flycheck helm-ag go-mode flyspell-lazy flycheck-ycmd expand-region evil-surround evil-nerd-commenter evil-leader evil-easymotion elscreen dockerfile-mode docker-compose-mode company-ycmd color-theme-approximate cmake-mode cider ag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
