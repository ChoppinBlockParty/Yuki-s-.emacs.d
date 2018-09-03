(use-package cider
  :ensure t
  :config
  (progn
    (add-hook 'cider-mode-hook #'eldoc-mode)
    (setq nrepl-log-messages nil)
  )
)

(provide 'config-cider)
