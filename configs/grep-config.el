
(defvar grep-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map compilation-minor-mode-map)
    (define-key map " " 'scroll-up-command)
    (define-key map [?\S-\ ] 'scroll-down-command)
    (define-key map "\^?" 'scroll-down-command)
    (define-key map "\C-c\C-f" 'next-error-follow-minor-mode)

    (define-key map "\r" 'compile-goto-error)  ;; ?
    (define-key map "n" 'next-error-no-select)
    (define-key map "p" 'previous-error-no-select)
    (define-key map "{" 'compilation-previous-file)
    (define-key map "}" 'compilation-next-file)
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
  "Keymap for grep buffers.
`compilation-minor-mode-map' is a cdr of this.")
