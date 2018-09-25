(use-package neotree
  :config
  (define-key evil-normal-state-map "`" 'neotree-toggle)

  (setq
    neo-theme 'icons
    neo-smart-open t
    neo-hidden-regexp-list '("\\.?tags?" "\\.\\([o]\\|py[doc]\\|elc\\|so\\|obj\\)$")
    neo-confirm-change-root 'y-or-n-p
    neo-confirm-create-file 'y-or-n-p
    neo-confirm-create-directory 'y-or-n-p
    neo-confirm-delete-file 'y-or-n-p
    neo-confirm-delete-directory-recursively 'y-or-n-p
    neo-confirm-kill-buffers-for-files-in-directory 'y-or-n-p
    )

  (defun neo-buffer--insert-header ()
    (let ((start (point)))
      (set-text-properties start (point) '(face neo-header-face)))
    (neo-buffer--newline-and-begin))

  (evil-set-initial-state 'neotree-mode 'normal)
  (evil-define-key 'normal neotree-mode-map
      (kbd "TAB")     (neotree-make-executor :dir-fn  'neo-open-dir)
      (kbd "RET")     (neotree-make-executor :file-fn 'neo-open-file :dir-fn  'neo-open-dir)
      (kbd "\\")      (neotree-make-executor :file-fn 'neo-open-file-vertical-split)
      (kbd "-")       (neotree-make-executor :file-fn 'neo-open-file-horizontal-split)
      (kbd "a")       (neotree-make-executor :file-fn 'neo-open-file-ace-window)
      (kbd "d")       (neotree-make-executor :dir-fn 'neo-open-dired)
      (kbd "u")       'neotree-quick-look
      (kbd "q")       'neotree-hide
      (kbd "k")       'neotree-previous-line
      (kbd "j")       'neotree-next-line
      (kbd "A")       'neotree-stretch-toggle
      (kbd "h")       'neotree-select-up-node
      (kbd "l")       'neotree-enter
      (kbd "o")       'neotree-enter
      (kbd "S")       'neotree-select-previous-sibling-node
      (kbd "s")       'neotree-select-next-sibling-node
      (kbd "O")       'neotree-open-file-in-system-application
      (kbd "C")       'neotree-create-node
      (kbd "D")       'neotree-delete-node
      (kbd "R")       'neotree-rename-node
      (kbd "g")       nil
      (kbd "gr")      'neotree-refresh
      (kbd "gt")      'neotree-change-root
      (kbd "gf")      'find-file-other-window
      (kbd "gcd")      'neotree-dir
      (kbd "z")       nil
      (kbd "zh")      'neotree-hidden-file-toggle
    )

)

(provide 'neotree-config)
