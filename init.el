;;; init --- Emacs startup script
;;; Commentary:
;;; Code:

;;; Configure emacs garbage collection from 0.76MB to 20MB
;;; Significantly improves performance.
;;; If the number is too big, initialization is fast, but
;;; emacs gets lags during usage.
(setq gc-cons-threshold 20000000)
(defun minibuffer-gc-cons-threshold-setup ()
  (setq-local init-minibuffer-gc-cons-threshold gc-cons-threshold)
  (setq                       gc-cons-threshold most-positive-fixnum))
(add-hook 'minibuffer-setup-hook #'minibuffer-gc-cons-threshold-setup)
(defun minibuffer-gc-cons-threshold-exit ()
  (when (local-variable-p 'init-minibuffer-gc-cons-threshold)
    (setq gc-cons-threshold init-minibuffer-gc-cons-threshold)))
(add-hook 'minibuffer-exit-hook  #'minibuffer-gc-cons-threshold-exit)

(add-to-list 'load-path (concat user-emacs-directory "configs"))

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

(setq url-proxy-services '(("no_proxy" . "^\\(localhost\\|127\\..*\\|192\\.168\\..*\\)")))

(require 'package)
(require 'gnutls)
(package-initialize)
(setq package-enable-at-startup nil)
(unless (package-installed-p 'use-package)
        (package-refresh-contents)
        (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(require 'base-config)
;;; Important enhancements to standard library `font-lock.el'
;;; Fixes issues with all-the-icons
;;; Seems like do not needed as of emacs 26.3.50
; (use-package font-lock+ :load-path "local/font-lock+")
(use-package flx) ;;; Gives Emacs a great fuzzy library. https://github.com/lewang/flx
(use-package smex) ;;; Enhances commands usage. https://github.com/nonsequitur/smex
(require 'window-splitting-config)
(require 'evil-config)
(require 'evil-extra-config)
(require 'generic-config)
(require 'all-the-icons-config)
(require 'window-numbering-config)
(require 'minibuffer-config)
(require 'dired-config)
(require 'completion-config)
(require 'shell-config)
(require 'vterm-config)
(require 'powerline-config)
(require 'color-theme-config)
(require 'highlight-config)
(require 'paren-config)
(require 'easymotion-config)
(require 'multi-cursor-config)
(require 'projectile-config)
(require 'wgrep-config)
(require 'rg-config)
(require 'flycheck-config)
(require 'flyspell-config)
;;; Why it has a path here?
(require 'helm-config (concat user-emacs-directory "configs/helm-config.el"))
(require 'ivy-config)
(require 'rainbow-mode-config)
(require 'magit-config)
(require 'pdf-config)
(require 'org-config)
(require 'file-modes-config)
(require 'latex-config)
(require 'tramp-config)
(require 'ediff-config)
(require 'flip-between-buffers-config)
(require 'other-stuff-config)
(require 'global-key-binding-config)

;;; Chooses random modes to obfuscate the current buffer, which can be used as a screensaver
;;; It is very fun, however consumes 10% cpu all the time, sad...
;; (re 'zone)
;; (zone-when-idle 120)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(lsp-mode kaolin-themes zerodark-theme xterm-color window-numbering wgrep vimrc-mode use-package spacemacs-theme smex rg rainbow-mode rainbow-delimiters powerline moe-theme markdown-mode magit ivy-rich iedit highlight-symbol highlight-operators helm-swoop helm-flycheck grandshell-theme go-mode flyspell-lazy flycheck-ycmd flx expand-region evil-surround evil-nerd-commenter evil-mc evil-matchit evil-leader evil-easymotion dockerfile-mode docker-compose-mode counsel-projectile company-ycmd cmake-mode clang-format auctex))
 '(warning-suppress-types '((comp) (comp) (comp) (comp) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-lead-face ((t (:background nil :foreground "#2192FF" :weight bold))))
 '(ivy-minibuffer-match-face-1 ((t (:background "dark orchid" :foreground "#eeeeee" :weight bold))))
 '(ivy-minibuffer-match-face-2 ((t (:background "dark orchid" :foreground "#eeeeee" :weight bold))))
 '(ivy-minibuffer-match-face-3 ((t (:background "dark orchid" :foreground "#eeeeee" :weight bold))))
 '(ivy-minibuffer-match-face-4 ((t (:background "dark orchid" :foreground "#eeeeee" :weight bold)))))
