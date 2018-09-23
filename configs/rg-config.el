;;; rg-config --- RG
;;; Commentary:
;;; Code:
(use-package rg
  :init
  (require 'compile)
  (defvar grep-mode-map
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map compilation-minor-mode-map)
      (define-key map " " 'scroll-up-command)
      (define-key map [?\S-\ ] 'scroll-down-command)
      (define-key map "\^?" 'scroll-down-command)
      (define-key map "g"   nil)
      (define-key map "gc"  'next-error-follow-minor-mode)

      (define-key map "\r" 'compile-goto-error)  ;; ?
      (define-key map "<" 'previous-error-no-select)
      (define-key map ">" 'next-error-no-select)
      (define-key map "{" 'compilation-next-file)
      (define-key map "}" 'compilation-previous-file)
      (define-key map "\t" 'compilation-next-error)
      (define-key map [backtab] 'compilation-previous-error)

      ;; Set up the menu-bar
      (define-key map [menu-bar grep]
        (cons "Grep" (make-sparse-keymap "Grep")))

      (define-key map [menu-bar grep compilation-kill-compilation]
        '(menu-item "Kill Grep" kill-compilation
        :help "Kill the currently running grep process"))
      (define-key map [menu-bar grep compilation-separator2] '("----"))
      (define-key map [menu-bar grep compilation-compile]
        '(menu-item "Compile..." compile
        :help "Compile the program including the current buffer.  Default: run `make'"))
      (define-key map [menu-bar grep compilation-rgrep]
        '(menu-item "Recursive grep..." rgrep
        :help "User-friendly recursive grep in directory tree"))
      (define-key map [menu-bar grep compilation-lgrep]
        '(menu-item "Local grep..." lgrep
        :help "User-friendly grep in a directory"))
      (define-key map [menu-bar grep compilation-grep-find]
        '(menu-item "Grep via Find..." grep-find
        :help "Run grep via find, with user-specified args"))
      (define-key map [menu-bar grep compilation-grep]
        '(menu-item "Another grep..." grep
        :help "Run grep, with user-specified args, and collect output in a buffer."))
      (define-key map [menu-bar grep compilation-recompile]
        '(menu-item "Repeat grep" recompile
        :help "Run grep again"))
      (define-key map [menu-bar grep compilation-separator2] '("----"))
      (define-key map [menu-bar grep compilation-first-error]
        '(menu-item "First Match" first-error
        :help "Restart at the first match, visit corresponding location"))
      (define-key map [menu-bar grep compilation-previous-error]
        '(menu-item "Previous Match" previous-error
        :help "Visit the previous match and corresponding location"))
      (define-key map [menu-bar grep compilation-next-error]
        '(menu-item "Next Match" next-error
        :help "Visit the next match and corresponding location"))
      map)
    "Keymap for grep buffers. `compilation-minor-mode-map' is a cdr of this.")
  (defvar rg-mode-map
    (let ((map (make-sparse-keymap)))
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
