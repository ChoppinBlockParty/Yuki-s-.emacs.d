;;; flycheck-config --- Brings magnificent checker at your disposal
;;; Commentary:
;;; Code:
(use-package flycheck
  :config
  (setq
    flycheck-mode-line nil
    )
  (setq-default
    flycheck-disabled-checkers
      (append flycheck-disabled-checkers
              '(javascript-jshint json-jsonlist))
    )
  (global-flycheck-mode)
  )

(provide 'flycheck-config)
;;; flycheck-config.el ends here
