;;; flycheck-config --- Brings magnificent checker at your disposal
;;; Commentary:
;;; Code:
(use-package flycheck
  :config
  (setq
    flycheck-mode-line nil
    )
  (global-flycheck-mode)
  )

(provide 'flycheck-config)
;;; flycheck-config.el ends here
