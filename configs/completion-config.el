;;; completion-config --- YCMD, company related configurations
;;; Commentary:
;;; Code:
(setq
  ;;; Non-nil means show help message in *Completions* buffer.
  completion-show-help nil
  )

;;; Big discussion on how run Tab completion
;;; https://github.com/company-mode/company-mode/pull/706
;;; Described in [Switch from AC](https://github.com/company-mode/company-mode/wiki/Switching-from-AC)
(use-package company
  :after (evil-config)
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  ;;; Goint to configure all myself
  ;; (add-hook 'after-init-hook 'company-tng-mode)

  (defun company-preview-if-not-tng-frontend (command)
      "`company-preview-frontend', but not when tng is active."
      (unless (and (eq command 'post-command)
                   company-selection-changed
                   (memq 'company-tng-frontend company-frontends))
      (company-preview-frontend command)))


  (setq
    ;; company-lighter-base ""
    company-idle-delay 0.0
    company-minimum-prefix-length 1
    company-selection-default nil
    company-require-match nil      ; cancel selections by typing non-matching character
    ;; company-tooltip-limit 20                       ; bigger popup window
    company-echo-delay 0                           ; remove annoying blinking
    ;; company-begin-commands '(self-insert-command)  ; start autocompletion only after typing
    ;; company-dabbrev-ignore-case nil
    ;; company-dabbrev-downcase nil
    company-frontends '(company-tng-frontend ;;; (TNG) Tab and Go
                        company-pseudo-tooltip-frontend
                        company-echo-metadata-frontend
                        ;; company-pseudo-tooltip-unless-just-one-frontend
                        ;;; Fancy frontend, but lagging
                        ;; company-preview-if-not-tng-frontend
                        )
    ;; Disable evil thing that makes all text completion in lower case.
    company-dabbrev-downcase nil
    )

  ;;; Argument auto-insertion does not work smoothly with completion auto-selection like with TNG frontend.
  (setq company-clang-insert-arguments nil
        company-semantic-insert-arguments nil
        company-rtags-insert-arguments nil
        lsp-enable-snippet nil
        company-ycmd-insert-arguments nil
        )

  (setq-default company-mode t)

  (advice-add #'eglot--snippet-expansion-fn :override #'ignore)

  (evil-define-key 'insert 'global (kbd "M-z") 'company-complete-common)
  )

(define-key company-active-map (kbd "C-h") 'company-show-doc-buffer)
(define-key company-active-map (kbd "C-l") 'company-show-location)
(define-key company-active-map (kbd "C-k") 'company-select-previous)
(define-key company-active-map (kbd "C-j") 'company-select-next)
(define-key company-active-map (kbd "M-k") 'company-select-previous)
(define-key company-active-map (kbd "M-j") 'company-select-next)
(define-key company-active-map (kbd "<down>") 'company-select-next-or-abort)
(define-key company-active-map (kbd "<up>") 'company-select-previous-or-abort)
(define-key company-active-map [down-mouse-1] 'ignore)
(define-key company-active-map [down-mouse-3] 'ignore)
(define-key company-active-map [mouse-1] 'ignore)
(define-key company-active-map [mouse-3] 'ignore)
(define-key company-active-map [up-mouse-1] 'ignore)
(define-key company-active-map [up-mouse-3] 'ignore)
(define-key company-active-map (kbd "[shift-tab]") 'company-select-previous)
(define-key company-active-map (kbd "S-TAB") 'company-select-previous)
(define-key company-active-map (kbd "<backtab>") 'company-select-previous)
(define-key company-active-map (kbd "[tab]") 'company-complete-common-or-cycle)
(define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
(define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
;;; Quits completion and removes last selection (works like a charm)
(define-key company-active-map (kbd "C-w") 'company-abort)
(define-key company-active-map [return] nil)
(define-key company-active-map (kbd "RET") nil)
(define-key company-active-map (kbd "[") nil)
(define-key company-active-map (kbd "]") nil)

(use-package ycmd
  :ensure nil
  :load-path "local/emacs-ycmd"
  :config
  (setq
    ycmd-mode-line-prefix ""
    ;;; ffs, everything is so complicated,..., this '-u' flag
    ;;; https://github.com/abingham/emacs-ycmd/issues/429
    ycmd-server-command `("python3" "-u" ,(file-truename "~/yuki/ycmd/ycmd"))
    ;;; Load, do not 'ask
    ycmd-extra-conf-handler 'load
    ycmd-global-config (file-truename "~/.ycm_extra_conf.py")
    ycmd-settings-json-filepath (concat user-emacs-directory "ycmd_default_settings.json")
    request-message-level -1
    )

  (modify-syntax-entry ?_ "w" ycmd-view-mode-syntax-table)
  (modify-syntax-entry ?_ "w" ycmd-fixit-mode-syntax-table)

  ;; (add-hook 'after-init-hook #'global-ycmd-mode)
  ;; (add-hook 'after-init-hook #'global-ycmd-mode)
  ;; (add-hook 'python-mode-hook 'ycmd-mode)
  (add-hook 'c-mode-hook 'ycmd-mode)
  (add-hook 'c++-mode-hook 'ycmd-mode)
  ;;; Not working as of 2021-01-18, switch to gocode (see below)
  ;; (add-hook 'go-mode-hook 'ycmd-mode)
  ;; (add-hook 'rjsx-mode-hook 'ycmd-mode)
  ;; (add-hook 'js2-mode-hook 'ycmd-mode)
  ;; (add-to-list 'ycmd-file-type-map '(rjsx-mode . ("javascript")))

  (evil-define-key 'normal ycmd-mode-map
     (kbd "SPC c") 'ycmd-goto-definition
     (kbd "SPC d") 'ycmd-goto-declaration)
  )

(evil-add-command-properties #'ycmd-goto :jump t)
(evil-add-command-properties #'ycmd-goto-include :jump t)
(evil-add-command-properties #'ycmd-goto-declaration :jump t)
(evil-add-command-properties #'ycmd-goto-definition :jump t)
(evil-add-command-properties #'ycmd-goto-implementation :jump t)
(evil-add-command-properties #'ycmd-goto-imprecise :jump t)
(evil-add-command-properties #'ycmd-goto-references :jump t)
(evil-add-command-properties #'ycmd-goto-type :jump t)

(use-package company-ycmd
  :ensure nil
  :load-path "local/emacs-ycmd"
  :config
    (company-ycmd-setup)
    ;; (add-to-list 'company-backends (company-mode/backend-with-yas 'company-ycmd))
  )

(use-package flycheck-ycmd
  :init
  (add-hook 'ycmd-mode-hook 'flycheck-ycmd-setup)
  :config

  (modify-syntax-entry ?_ "w" flycheck-error-list-mode-syntax-table)


  ;;; Make sure the flycheck cache sees the parse results
  (add-hook 'ycmd-file-parse-result-hook 'flycheck-ycmd--cache-parse-results)

  ;;; Add the ycmd checker to the list of available checkers
  (add-to-list 'flycheck-checkers 'ycmd)
  )

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "M-z")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
        (python-mode . lsp)
        ;; if you want which-key integration
        ;;(lsp-mode . lsp-enable-which-key-integration))
        )
  )

(provide 'completion-config)
;;; completion-config.el ends here
