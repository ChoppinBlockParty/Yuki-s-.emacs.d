(use-package neotree
  :ensure t
  :config
  (progn
    (global-set-key [f4] 'neotree-toggle)

    (defun neo-buffer--insert-header ()
      (let ((start (point)))
        (set-text-properties start (point) '(face neo-header-face)))
      (neo-buffer--newline-and-begin))

    (after 'evil
        (evil-set-initial-state 'neotree-mode 'normal)
        (evil-define-key 'normal neotree-mode-map
            (kbd "RET") 'neotree-enter
            (kbd "c")   'neotree-create-node
            (kbd "r")   'neotree-rename-node
            (kbd "d")   'neotree-delete-node
            (kbd "j")   'neotree-next-line
            (kbd "k")   'neotree-previous-line
            (kbd "SPC") 'neotree-change-root
            (kbd "q")   'neotree-hide
            (kbd "l")   'neotree-enter
        )
    )

  )
)

(provide 'neotree-config)
