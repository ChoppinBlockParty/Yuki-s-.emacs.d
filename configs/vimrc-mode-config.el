(use-package vimrc-mode
  :ensure t
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.vim\\(rc\\)?\\'" . vimrc-mode))
    )
  )

(provide 'vimrc-mode-config)
