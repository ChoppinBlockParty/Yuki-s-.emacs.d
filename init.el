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
(use-package font-lock+ :load-path "local/font-lock+")
(use-package flx) ;;; Gives Emacs a great fuzzy library. https://github.com/lewang/flx
(use-package smex) ;;; Enhances commands usage. https://github.com/nonsequitur/smex
(require 'evil-config)
(require 'generic-config)
(require 'global-key-binding-config)
(require 'all-the-icons-config)
(require 'window-numbering-config)
(require 'minibuffer-config)
(require 'dired-config)
(require 'completion-config)
(require 'shell-config)
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
(require 'helm-config (concat user-emacs-directory "configs/helm-config.el"))
(require 'ivy-config)
(require 'rainbow-mode-config)
(require 'magit-config)
(require 'pdf-config)
(require 'org-config)
(require 'file-modes-config)

;;; Chooses random modes to obfuscate the current buffer, which can be used as a screensaver
;;; It is very fun, however consumes 100% cpu all the time, sad...
;; (re 'zone)
;; (zone-when-idle 120)

;;; https://emacs.stackexchange.com/questions/9583/how-to-treat-underscore-as-part-of-the-word
(modify-syntax-entry ?_ "w" (standard-syntax-table))
(modify-syntax-entry ?- "w" (standard-syntax-table))
(modify-syntax-entry ?_ "w" emacs-lisp-mode-syntax-table)
(modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table)
(modify-syntax-entry ?_ "w" xml-syntax-table)
(modify-syntax-entry ?- "w" xml-syntax-table)
(modify-syntax-entry ?_ "w" prog-mode-syntax-table)
(modify-syntax-entry ?- "w" prog-mode-syntax-table)
(modify-syntax-entry ?_ "w" url-parse-syntax-table)
(modify-syntax-entry ?- "w" url-parse-syntax-table)
(modify-syntax-entry ?_ "w" help-mode-syntax-table)
(modify-syntax-entry ?- "w" help-mode-syntax-table)
(modify-syntax-entry ?_ "w" text-mode-syntax-table)
(modify-syntax-entry ?- "w" text-mode-syntax-table)
(modify-syntax-entry ?_ "w" font-lock-syntax-table)
(modify-syntax-entry ?- "w" font-lock-syntax-table)
(modify-syntax-entry ?_ "w" lisp-mode-syntax-table)
(modify-syntax-entry ?- "w" lisp-mode-syntax-table)
(modify-syntax-entry ?_ "w" Info-mode-syntax-table)
(modify-syntax-entry ?- "w" Info-mode-syntax-table)
(modify-syntax-entry ?_ "w" occur-mode-syntax-table)
(modify-syntax-entry ?- "w" occur-mode-syntax-table)
(modify-syntax-entry ?_ "w" comint-mode-syntax-table)
(modify-syntax-entry ?- "w" comint-mode-syntax-table)
(modify-syntax-entry ?_ "w" special-mode-syntax-table)
(modify-syntax-entry ?- "w" special-mode-syntax-table)
(modify-syntax-entry ?_ "w" epa-key-mode-syntax-table)
(modify-syntax-entry ?- "w" epa-key-mode-syntax-table)
(modify-syntax-entry ?_ "w" epa-info-mode-syntax-table)
(modify-syntax-entry ?- "w" epa-info-mode-syntax-table)
(modify-syntax-entry ?_ "w" Info-edit-mode-syntax-table)
(modify-syntax-entry ?- "w" Info-edit-mode-syntax-table)
(modify-syntax-entry ?_ "w" occur-edit-mode-syntax-table)
(modify-syntax-entry ?- "w" occur-edit-mode-syntax-table)
(modify-syntax-entry ?_ "w" change-log-mode-syntax-table)
(modify-syntax-entry ?- "w" change-log-mode-syntax-table)
(modify-syntax-entry ?_ "w" Buffer-menu-mode-syntax-table)
(modify-syntax-entry ?- "w" Buffer-menu-mode-syntax-table)
(modify-syntax-entry ?_ "w" process-menu-mode-syntax-table)
(modify-syntax-entry ?- "w" process-menu-mode-syntax-table)
(modify-syntax-entry ?_ "w" edit-abbrevs-mode-syntax-table)
(modify-syntax-entry ?- "w" edit-abbrevs-mode-syntax-table)
(modify-syntax-entry ?_ "w" package-menu-mode-syntax-table)
(modify-syntax-entry ?- "w" package-menu-mode-syntax-table)
(modify-syntax-entry ?_ "w" tabulated-list-mode-syntax-table)
(modify-syntax-entry ?- "w" tabulated-list-mode-syntax-table)
(modify-syntax-entry ?_ "w" completion-list-mode-syntax-table)
(modify-syntax-entry ?- "w" completion-list-mode-syntax-table)
(modify-syntax-entry ?_ "w" messages-buffer-mode-syntax-table)
(modify-syntax-entry ?- "w" messages-buffer-mode-syntax-table)
(modify-syntax-entry ?_ "w" lisp-interaction-mode-syntax-table)
(modify-syntax-entry ?- "w" lisp-interaction-mode-syntax-table)
(modify-syntax-entry ?_ "w" epa-key-list-mode-syntax-table)
(modify-syntax-entry ?- "w" epa-key-list-mode-syntax-table)
(modify-syntax-entry ?_ "w" evil-list-view-mode-syntax-table)
(modify-syntax-entry ?- "w" evil-list-view-mode-syntax-table)
(modify-syntax-entry ?_ "w" elisp-byte-code-mode-syntax-table)
(modify-syntax-entry ?- "w" elisp-byte-code-mode-syntax-table)
(modify-syntax-entry ?_ "w" evil-command-window-mode-syntax-table)
(modify-syntax-entry ?- "w" evil-command-window-mode-syntax-table)
(modify-syntax-entry ?_ "w" use-package-statistics-mode-syntax-table)
(modify-syntax-entry ?- "w" use-package-statistics-mode-syntax-table)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (kaolin-themes zerodark-theme xterm-color window-numbering wgrep vimrc-mode use-package spacemacs-theme smex rg rainbow-mode rainbow-delimiters powerline moe-theme markdown-mode magit ivy-rich iedit highlight-symbol highlight-operators helm-swoop helm-flycheck grandshell-theme go-mode flyspell-lazy flycheck-ycmd flx expand-region evil-surround evil-nerd-commenter evil-mc evil-matchit evil-leader evil-easymotion dockerfile-mode docker-compose-mode counsel-projectile company-ycmd cmake-mode clang-format auctex))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-lead-face ((t (:background nil :foreground "#ff2929" :weight bold)))))
