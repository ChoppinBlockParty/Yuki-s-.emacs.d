(use-package markdown-mode
  :ensure t
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
    (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
    (add-to-list 'auto-mode-alist '("\\.page\\'" . markdown-mode))
    (add-hook 'markdown-mode-hook 'visual-line-mode)
    )
  )

(provide 'markdown-mode-config)
