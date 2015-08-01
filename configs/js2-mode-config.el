(use-package js2-mode
  :ensure t
  :config
  (progn
    (add-hook 'js-mode-hook 'js2-minor-mode)
    ;; You may also want to hook it in for shell scripts running via node.js:
    (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
    )
  )

(provide 'js2-mode-config)
