;;; init.el -- Emacs startup script

;;; Commentary:

;;; Code:

;;; Configure emacs garbage collection from 0.76MB to 20MB
;;; Significantly improves performance.
;;; If the number is too big, initialization is fast, but
;;; emacs gets lags during usage.
(setq gc-cons-threshold 20000000)

(add-to-list 'load-path
             (concat user-emacs-directory
                     (convert-standard-filename "configs")))
(add-to-list 'load-path
             (concat user-emacs-directory
                     (convert-standard-filename "local/protobuf")))
;; (add-to-list 'load-path
;;              (concat user-emacs-directory
;;                      (convert-standard-filename "local/telephone-line")))
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

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
(use-package all-the-icons :ensure t)
(require 'configs-base)
(require 'flx-config)
(require 'smex-config)
(require 'core-funcs)
(require 'window-numbering-config)
;; (require 'cua-config)
(require 'core-micro-state)
;; (require 'config-color)
(require 'highlight-config)
(require 'projectile-config)
(require 'config-cl-lib)
(require 'configs-evil)
(require 'minibuffer-config)
(require 'configs-dired)
(require 'auto-fill-mode-config)
;; (require 'ag-config)
(require 'configs-helm)
(require 'multi-cursor-config)
(require 'ivy-config)
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
(require 'magit-config)

;; (require 'config-cider)
;; There is ycmd-jedi, do not this one
;; (require 'jedi-config)
(require 'vimrc-mode-config)
(require 'docker-config)

(use-package highlight-symbol
  :ensure t
  :config
  (progn
    (highlight-symbol-mode t)
    (setq highlight-symbol-on-naviagtion-p t
          highlight-symbol-idle-delay 0
      )
    (global-set-key [f3] 'highlight-symbol)
    (global-set-key [(ctrl f3)] 'highlight-symbol-remove-all)
    ))

 (defun my-formatting ()
   ""
   (interactive)
   (cond
     ((equal major-mode 'go-mode) (gofmt))
     ((equal major-mode 'c++-mode) (clang-format-buffer))
     (t nil))
   )

    (define-key evil-normal-state-map (kbd "SPC s") 'my-formatting)

  (require 'protobuf-mode)
  (add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))

(use-package cmake-mode :ensure t)

(use-package go-mode
  :ensure t
  :after (evil)
  :config
  (progn
    (add-to-list `auto-mode-alist '("\\.go\\'" . go-mode))
    ))

(use-package clang-format :ensure t)

;; (use-package multiple-cursors
;;   :ensure t
;;   :after(evil flyspell)
;;   :config
;;   (defun my-evil-mc-make-all-cursors ()
;;     (interactive)
;;     (if (evil-mc-has-cursors-p)
;;         (evil-mc-undo-all-cursors)
;;         (evil-mc-make-all-cursors)
;;       )
;;     )
;;   (defvar evil-mc-key-map
;;     (let ((map (make-sparse-keymap))
;;           (keys '(("R" . my-evil-mc-make-all-cursors))))
;;     (dolist (key-data keys)
;;       (evil-define-key 'normal map (kbd (car key-data)) (cdr key-data))
;;       (evil-define-key 'visual map (kbd (car key-data)) (cdr key-data)))
;;     map))
;;     (global-evil-mc-mode 1)
;;   )

;;; Chooses random modes to obfuscate the current buffer, which can be used as a screensaver
;;; It is very fun, however consumes 100% cpu all the time, sad...
;; (re 'zone)
;; (zone-when-idle 120)

(modify-syntax-entry ?_ "w" (standard-syntax-table))
(modify-syntax-entry ?- "w" (standard-syntax-table))
(modify-syntax-entry ?_ "w" emacs-lisp-mode-syntax-table)
(modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table)
(modify-syntax-entry ?_ "w" xml-syntax-table)
(modify-syntax-entry ?- "w" xml-syntax-table)
(modify-syntax-entry ?_ "w" mml-syntax-table)
(modify-syntax-entry ?- "w" mml-syntax-table)
(modify-syntax-entry ?_ "w" rfc2047-syntax-table)
(modify-syntax-entry ?- "w" rfc2047-syntax-table)
(modify-syntax-entry ?_ "w" c-mode-syntax-table)
(modify-syntax-entry ?- "w" c-mode-syntax-table)
(modify-syntax-entry ?_ "w" go-mode-syntax-table)
(modify-syntax-entry ?- "w" go-mode-syntax-table)
(modify-syntax-entry ?_ "w" sh-mode-syntax-table)
(modify-syntax-entry ?- "w" sh-mode-syntax-table)
(modify-syntax-entry ?_ "w" org-mode-syntax-table)
(modify-syntax-entry ?- "w" org-mode-syntax-table)
(modify-syntax-entry ?_ "w" gfm-mode-syntax-table)
(modify-syntax-entry ?- "w" gfm-mode-syntax-table)
(modify-syntax-entry ?_ "w" idl-mode-syntax-table)
(modify-syntax-entry ?- "w" idl-mode-syntax-table)
(modify-syntax-entry ?_ "w" rst-mode-syntax-table)
(modify-syntax-entry ?- "w" rst-mode-syntax-table)
(modify-syntax-entry ?_ "w" prog-mode-syntax-table)
(modify-syntax-entry ?- "w" prog-mode-syntax-table)
(modify-syntax-entry ?_ "w" java-mode-syntax-table)
(modify-syntax-entry ?- "w" java-mode-syntax-table)
(modify-syntax-entry ?_ "w" url-parse-syntax-table)
(modify-syntax-entry ?- "w" url-parse-syntax-table)
(modify-syntax-entry ?_ "w" help-mode-syntax-table)
(modify-syntax-entry ?- "w" help-mode-syntax-table)
(modify-syntax-entry ?_ "w" grep-mode-syntax-table)
(modify-syntax-entry ?- "w" grep-mode-syntax-table)
(modify-syntax-entry ?_ "w" diff-mode-syntax-table)
(modify-syntax-entry ?- "w" diff-mode-syntax-table)
(modify-syntax-entry ?_ "w" text-mode-syntax-table)
(modify-syntax-entry ?- "w" text-mode-syntax-table)
(modify-syntax-entry ?_ "w" font-lock-syntax-table)
(modify-syntax-entry ?- "w" font-lock-syntax-table)
(modify-syntax-entry ?_ "w" lisp-mode-syntax-table)
(modify-syntax-entry ?- "w" lisp-mode-syntax-table)
(modify-syntax-entry ?_ "w" pike-mode-syntax-table)
(modify-syntax-entry ?- "w" pike-mode-syntax-table)
(modify-syntax-entry ?_ "w" Info-mode-syntax-table)
(modify-syntax-entry ?- "w" Info-mode-syntax-table)
(modify-syntax-entry ?_ "w" objc-mode-syntax-table)
(modify-syntax-entry ?- "w" objc-mode-syntax-table)
(modify-syntax-entry ?_ "w" shell-mode-syntax-table)
(modify-syntax-entry ?- "w" shell-mode-syntax-table)
(modify-syntax-entry ?_ "w" occur-mode-syntax-table)
(modify-syntax-entry ?- "w" occur-mode-syntax-table)
(modify-syntax-entry ?_ "w" vimrc-mode-syntax-table)
(modify-syntax-entry ?- "w" vimrc-mode-syntax-table)
(modify-syntax-entry ?_ "w" ietf-drums-syntax-table)
(modify-syntax-entry ?- "w" ietf-drums-syntax-table)
(modify-syntax-entry ?_ "w" godoc-mode-syntax-table)
(modify-syntax-entry ?- "w" godoc-mode-syntax-table)
(modify-syntax-entry ?_ "w" magit-mode-syntax-table)
(modify-syntax-entry ?- "w" magit-mode-syntax-table)
(modify-syntax-entry ?_ "w" cmake-mode-syntax-table)
(modify-syntax-entry ?- "w" cmake-mode-syntax-table)
(modify-syntax-entry ?_ "w" eshell-mode-syntax-table)
(modify-syntax-entry ?- "w" eshell-mode-syntax-table)
(modify-syntax-entry ?_ "w" comint-mode-syntax-table)
(modify-syntax-entry ?- "w" comint-mode-syntax-table)
(modify-syntax-entry ?_ "w" special-mode-syntax-table)
(modify-syntax-entry ?- "w" special-mode-syntax-table)
(modify-syntax-entry ?_ "w" outline-mode-syntax-table)
(modify-syntax-entry ?- "w" outline-mode-syntax-table)
(modify-syntax-entry ?_ "w" ibuffer-mode-syntax-table)
(modify-syntax-entry ?- "w" ibuffer-mode-syntax-table)
(modify-syntax-entry ?_ "w" c-identifier-syntax-table)
(modify-syntax-entry ?- "w" c-identifier-syntax-table)
;; (modify-syntax-entry ?_ "w" clojure-mode-syntax-table)
;; (modify-syntax-entry ?- "w" clojure-mode-syntax-table)
(modify-syntax-entry ?_ "w" message-mode-syntax-table)
(modify-syntax-entry ?- "w" message-mode-syntax-table)
(modify-syntax-entry ?_ "w" markdown-mode-syntax-table)
(modify-syntax-entry ?- "w" markdown-mode-syntax-table)
;; (modify-syntax-entry ?_ "w" clojurec-mode-syntax-table)
;; (modify-syntax-entry ?- "w" clojurec-mode-syntax-table)
(modify-syntax-entry ?_ "w" calendar-mode-syntax-table)
(modify-syntax-entry ?- "w" calendar-mode-syntax-table)
(modify-syntax-entry ?_ "w" protobuf-mode-syntax-table)
(modify-syntax-entry ?- "w" protobuf-mode-syntax-table)
(modify-syntax-entry ?_ "w" dockerfile-mode-syntax-table)
(modify-syntax-entry ?- "w" dockerfile-mode-syntax-table)
;; (modify-syntax-entry ?_ "w" clojurescript-mode-syntax-table)
;; (modify-syntax-entry ?- "w" clojurescript-mode-syntax-table)
(modify-syntax-entry ?_ "w" c++-mode-syntax-table)
(modify-syntax-entry ?- "w" c++-mode-syntax-table)
(modify-syntax-entry ?_ "w" c++-template-syntax-table)
(modify-syntax-entry ?- "w" c++-template-syntax-table)
(modify-syntax-entry ?_ "w" c-no-parens-syntax-table)
(modify-syntax-entry ?- "w" c-no-parens-syntax-table)
(modify-syntax-entry ?_ "w" rst-toc-mode-syntax-table)
(modify-syntax-entry ?- "w" rst-toc-mode-syntax-table)
(modify-syntax-entry ?_ "w" epa-key-mode-syntax-table)
(modify-syntax-entry ?- "w" epa-key-mode-syntax-table)
(modify-syntax-entry ?_ "w" helm-ag-mode-syntax-table)
(modify-syntax-entry ?- "w" helm-ag-mode-syntax-table)
(modify-syntax-entry ?_ "w" log-edit-mode-syntax-table)
(modify-syntax-entry ?- "w" log-edit-mode-syntax-table)
(modify-syntax-entry ?_ "w" epa-info-mode-syntax-table)
(modify-syntax-entry ?- "w" epa-info-mode-syntax-table)
(modify-syntax-entry ?_ "w" gfm-view-mode-syntax-table)
(modify-syntax-entry ?- "w" gfm-view-mode-syntax-table)
(modify-syntax-entry ?_ "w" magit-log-mode-syntax-table)
(modify-syntax-entry ?- "w" magit-log-mode-syntax-table)
(modify-syntax-entry ?_ "w" ycmd-view-mode-syntax-table)
(modify-syntax-entry ?- "w" ycmd-view-mode-syntax-table)
(modify-syntax-entry ?_ "w" url-parse-args-syntax-table)
(modify-syntax-entry ?- "w" url-parse-args-syntax-table)
(modify-syntax-entry ?_ "w" helm-grep-mode-syntax-table)
(modify-syntax-entry ?- "w" helm-grep-mode-syntax-table)
(modify-syntax-entry ?_ "w" Info-edit-mode-syntax-table)
(modify-syntax-entry ?- "w" Info-edit-mode-syntax-table)
(modify-syntax-entry ?_ "w" ivy-occur-mode-syntax-table)
(modify-syntax-entry ?- "w" ivy-occur-mode-syntax-table)
(modify-syntax-entry ?_ "w" occur-edit-mode-syntax-table)
(modify-syntax-entry ?- "w" occur-edit-mode-syntax-table)
(modify-syntax-entry ?_ "w" change-log-mode-syntax-table)
(modify-syntax-entry ?- "w" change-log-mode-syntax-table)
(modify-syntax-entry ?_ "w" ycmd-fixit-mode-syntax-table)
(modify-syntax-entry ?- "w" ycmd-fixit-mode-syntax-table)
(modify-syntax-entry ?_ "w" url-cookie-mode-syntax-table)
(modify-syntax-entry ?- "w" url-cookie-mode-syntax-table)
(modify-syntax-entry ?_ "w" magit-refs-mode-syntax-table)
(modify-syntax-entry ?- "w" magit-refs-mode-syntax-table)
(modify-syntax-entry ?_ "w" helm-major-mode-syntax-table)
(modify-syntax-entry ?- "w" helm-major-mode-syntax-table)
(modify-syntax-entry ?_ "w" magit-diff-mode-syntax-table)
(modify-syntax-entry ?- "w" magit-diff-mode-syntax-table)
(modify-syntax-entry ?_ "w" magit-stash-mode-syntax-table)
(modify-syntax-entry ?- "w" magit-stash-mode-syntax-table)
(modify-syntax-entry ?_ "w" helm-moccur-mode-syntax-table)
(modify-syntax-entry ?- "w" helm-moccur-mode-syntax-table)
(modify-syntax-entry ?_ "w" Buffer-menu-mode-syntax-table)
(modify-syntax-entry ?- "w" Buffer-menu-mode-syntax-table)
(modify-syntax-entry ?_ "w" magit-popup-mode-syntax-table)
(modify-syntax-entry ?- "w" magit-popup-mode-syntax-table)
(modify-syntax-entry ?_ "w" magit-cherry-mode-syntax-table)
(modify-syntax-entry ?- "w" magit-cherry-mode-syntax-table)
(modify-syntax-entry ?_ "w" magit-reflog-mode-syntax-table)
(modify-syntax-entry ?- "w" magit-reflog-mode-syntax-table)
(modify-syntax-entry ?_ "w" magit-status-mode-syntax-table)
(modify-syntax-entry ?- "w" magit-status-mode-syntax-table)
(modify-syntax-entry ?_ "w" process-menu-mode-syntax-table)
(modify-syntax-entry ?- "w" process-menu-mode-syntax-table)
(modify-syntax-entry ?_ "w" edit-abbrevs-mode-syntax-table)
(modify-syntax-entry ?- "w" edit-abbrevs-mode-syntax-table)
(modify-syntax-entry ?_ "w" package-menu-mode-syntax-table)
(modify-syntax-entry ?- "w" package-menu-mode-syntax-table)
(modify-syntax-entry ?_ "w" markdown-view-mode-syntax-table)
(modify-syntax-entry ?- "w" markdown-view-mode-syntax-table)
(modify-syntax-entry ?_ "w" magit-stashes-mode-syntax-table)
(modify-syntax-entry ?- "w" magit-stashes-mode-syntax-table)
(modify-syntax-entry ?_ "w" magit-process-mode-syntax-table)
(modify-syntax-entry ?- "w" magit-process-mode-syntax-table)
(modify-syntax-entry ?_ "w" mailcap-parse-args-syntax-table)
(modify-syntax-entry ?- "w" mailcap-parse-args-syntax-table)
(modify-syntax-entry ?_ "w" magit-repolist-mode-syntax-table)
(modify-syntax-entry ?- "w" magit-repolist-mode-syntax-table)
(modify-syntax-entry ?_ "w" magit-revision-mode-syntax-table)
(modify-syntax-entry ?- "w" magit-revision-mode-syntax-table)
(modify-syntax-entry ?_ "w" tabulated-list-mode-syntax-table)
(modify-syntax-entry ?- "w" tabulated-list-mode-syntax-table)
(modify-syntax-entry ?_ "w" docker-compose-mode-syntax-table)
(modify-syntax-entry ?- "w" docker-compose-mode-syntax-table)
(modify-syntax-entry ?_ "w" completion-list-mode-syntax-table)
(modify-syntax-entry ?- "w" completion-list-mode-syntax-table)
(modify-syntax-entry ?_ "w" messages-buffer-mode-syntax-table)
(modify-syntax-entry ?- "w" messages-buffer-mode-syntax-table)
(modify-syntax-entry ?_ "w" lisp-interaction-mode-syntax-table)
(modify-syntax-entry ?- "w" lisp-interaction-mode-syntax-table)
(modify-syntax-entry ?_ "w" minibuffer-inactive-mode-syntax-table)
(modify-syntax-entry ?- "w" minibuffer-inactive-mode-syntax-table)
(modify-syntax-entry ?_ "w" epa-key-list-mode-syntax-table)
(modify-syntax-entry ?- "w" epa-key-list-mode-syntax-table)
(modify-syntax-entry ?_ "w" evil-list-view-mode-syntax-table)
(modify-syntax-entry ?- "w" evil-list-view-mode-syntax-table)
(modify-syntax-entry ?_ "w" ivy-occur-grep-mode-syntax-table)
(modify-syntax-entry ?- "w" ivy-occur-grep-mode-syntax-table)
(modify-syntax-entry ?_ "w" elisp-byte-code-mode-syntax-table)
(modify-syntax-entry ?- "w" elisp-byte-code-mode-syntax-table)
(modify-syntax-entry ?_ "w" magit-log-select-mode-syntax-table)
(modify-syntax-entry ?- "w" magit-log-select-mode-syntax-table)
(modify-syntax-entry ?_ "w" select-tags-table-mode-syntax-table)
(modify-syntax-entry ?- "w" select-tags-table-mode-syntax-table)
(modify-syntax-entry ?_ "w" org-mode-transpose-word-syntax-table)
(modify-syntax-entry ?- "w" org-mode-transpose-word-syntax-table)
(modify-syntax-entry ?_ "w" evil-command-window-mode-syntax-table)
(modify-syntax-entry ?- "w" evil-command-window-mode-syntax-table)
(modify-syntax-entry ?_ "w" flycheck-error-list-mode-syntax-table)
(modify-syntax-entry ?- "w" flycheck-error-list-mode-syntax-table)
(modify-syntax-entry ?_ "w" magit-merge-preview-mode-syntax-table)
(modify-syntax-entry ?- "w" magit-merge-preview-mode-syntax-table)
(modify-syntax-entry ?_ "w" use-package-statistics-mode-syntax-table)
(modify-syntax-entry ?- "w" use-package-statistics-mode-syntax-table)
(modify-syntax-entry ?_ "w" git-commit-elisp-text-mode-syntax-table)
(modify-syntax-entry ?- "w" git-commit-elisp-text-mode-syntax-table)
;;   ;;; After you do that, ciw works as you want it to, such that it will select all of
;;   ;;; abc_def_ghi rather than just def.
;;   ;;; Doing it this way, however, may be overkill, especially if you only want the _
;;   ;;; to count as part of the word for the text object. Instead, you can advise
;;   ;;; evil-inner-word as follows:
;;   ;;; https://emacs.stackexchange.com/questions/9583/how-to-treat-underscore-as-part-of-the-word
;;   ;; (print table)
;;   ;; (modify-syntax-entry ?_ "w" emacs-lisp-mode-syntax-table)
;;   (modify-syntax-entry ?- "w" tt)
;; )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (projectile ace-jump-buffer window-numbering visual-regexp vimrc-mode use-package rainbow-mode rainbow-delimiters powerline moe-theme markdown-mode magit highlight-symbol highlight-operators helm-themes helm-swoop helm-projectile helm-flycheck helm-ag go-mode flyspell-lazy flycheck-ycmd expand-region evil-surround evil-nerd-commenter evil-leader evil-easymotion elscreen dockerfile-mode docker-compose-mode company-ycmd color-theme-approximate cmake-mode cider ag)))
 '(speedbar-file-unshown-regexp
   "\\.o\\'\\|~\\'\\|\\.bin\\'\\|\\.lbin\\'\\|\\.so\\'\\|\\.a\\'\\|\\.ln\\'\\|\\.blg\\'\\|\\.bbl\\'\\|\\.elc\\'\\|\\.lof\\'\\|\\.glo\\'\\|\\.idx\\'\\|\\.lot\\'\\|\\.svn/\\'\\|\\.hg/\\'\\|\\.git/\\'\\|\\.bzr/\\'\\|CVS/\\'\\|_darcs/\\'\\|_MTN/\\'\\|\\.fmt\\'\\|\\.tfm\\'\\|\\.class\\'\\|\\.fas\\'\\|\\.lib\\'\\|\\.mem\\'\\|\\.x86f\\'\\|\\.sparcf\\'\\|\\.dfsl\\'\\|\\.pfsl\\'\\|\\.d64fsl\\'\\|\\.p64fsl\\'\\|\\.lx64fsl\\'\\|\\.lx32fsl\\'\\|\\.dx64fsl\\'\\|\\.dx32fsl\\'\\|\\.fx64fsl\\'\\|\\.fx32fsl\\'\\|\\.sx64fsl\\'\\|\\.sx32fsl\\'\\|\\.wx64fsl\\'\\|\\.wx32fsl\\'\\|\\.fasl\\'\\|\\.ufsl\\'\\|\\.fsl\\'\\|\\.dxl\\'\\|\\.lo\\'\\|\\.la\\'\\|\\.gmo\\'\\|\\.mo\\'\\|\\.toc\\'\\|\\.aux\\'\\|\\.cp\\'\\|\\.fn\\'\\|\\.ky\\'\\|\\.pg\\'\\|\\.tp\\'\\|\\.vr\\'\\|\\.cps\\'\\|\\.fns\\'\\|\\.kys\\'\\|\\.pgs\\'\\|\\.tps\\'\\|\\.vrs\\'\\|\\.pyc\\'\\|\\.pyo\\'\\|#[^#]+#$")
 '(speedbar-visiting-file-hook nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ivy-minibuffer-match-face-1 ((t (:background "dark orchid" :foreground "#eeeeee" :weight bold))))
 '(ivy-minibuffer-match-face-2 ((t (:background "dark orchid" :foreground "#eeeeee" :weight bold))))
 '(ivy-minibuffer-match-face-3 ((t (:background "dark orchid" :foreground "#eeeeee" :weight bold))))
 '(ivy-minibuffer-match-face-4 ((t (:background "dark orchid" :foreground "#eeeeee" :weight bold)))))
