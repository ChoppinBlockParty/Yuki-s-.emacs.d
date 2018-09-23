;;; rg-config --- RG
;;; Commentary:
;;; Code:
(setq special-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (suppress-keymap map)
    (define-key map "q" 'quit-window)
    ;; (define-key map " " 'scroll-up-command)
    ;; (define-key map [?\S-\ ] 'scroll-down-command)
    ;; (define-key map "\C-?" 'scroll-down-command)
    ;; (define-key map "?" 'describe-mode)
    ;; (define-key map "h" 'describe-mode)
    (define-key map "g" nil)
    (define-key map "gr" 'revert-buffer)
    (define-key map "gg" 'beginning-of-buffer)
    (define-key map "G" 'end-of-buffer)
    map))

(use-package compile
  :ensure nil
  :demand t
  :init
  (defvar compilation-minor-mode-map
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map special-mode-map)
      (define-key map [mouse-2] 'compile-goto-error)
      (define-key map [follow-link] 'mouse-face)
      (define-key map "gt" 'compile-goto-error)
      (define-key map "u" 'compilation-display-error)
      (define-key map "\C-c\C-c" 'kill-compilation)
      (define-key map "<" 'compilation-previous-error)
      (define-key map ">" 'compilation-next-error)
      (define-key map "}" 'compilation-previous-file)
      (define-key map "{" 'compilation-next-file)
      (define-key map "gr" 'recompile) ; revert
      map)
    )
  )

(use-package grep
  :ensure nil
  :init
  (defvar grep-mode-map
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map compilation-minor-mode-map)
      (define-key map " " 'scroll-up-command)
      (define-key map [?\S-\ ] 'scroll-down-command)
      (define-key map "\^?" 'scroll-down-command)
      (define-key map "gc" 'next-error-follow-minor-mode)
      (define-key map "\r" 'compile-goto-error)  ;; ?
      (define-key map "\t"   (lambda() (interactive) (compile-goto-error) (select-window (previous-window))))
      (define-key map "<" 'previous-error-no-select)
      (define-key map ">" 'next-error-no-select)
      (define-key map "}" 'compilation-previous-file)
      (define-key map "{" 'compilation-next-file)
      ;; (define-key map "\t" 'compilation-next-error)
      (define-key map [backtab] 'compilation-previous-error)
      map)
    )
  )

(use-package rg
  :init
  (defvar rg-mode-map
    (let ((map (copy-keymap grep-mode-map)))
      (define-key map "c"    'rg-rerun-toggle-case)
      (define-key map "gt"   'rg-rerun-change-dir)
      (define-key map "f"    'rg-rerun-change-files)
      (define-key map "r"    'rg-recompile)
      (define-key map "i"    'rg-rerun-toggle-ignore)
      (define-key map "o"    'rg-list-searches)
      (define-key map "p"    'rg-rerun-change-regexp)
      (define-key map "s"    'rg-save-search)
      (define-key map "S"    'rg-save-search-as-name)
      (define-key map "l"    'rg-rerun-change-literal)
      (define-key map "\C-k" 'rg-prev-file)
      (define-key map "\C-j" 'rg-next-file)
      map)
  "The global keymap for `rg'.")
  :config
  (setq
    rg-keymap-prefix nil
    rg-ignore-case 'smart
    )
  (evil-set-initial-state 'rg-mode 'normal)
  )

(provide 'rg-config)
;;; rg-config.el ends here
