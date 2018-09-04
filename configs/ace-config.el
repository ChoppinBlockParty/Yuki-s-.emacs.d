

(use-package ace-jump-buffer
  :ensure t
  :config
  ;; When non-nil, a gray background will be added during the selection.
  (setq avy-background t)
  ;; at: - single character path on target, obscuring the target.
  (setq avy-style 'at)
  ;; Any lower-case letter or number.  Numbers are specified in the keyboard
  ;; number-row order, so that the candidate following '9' will be '0'.
  (setq avy-keys (nconc (number-sequence ?a ?z)
                        (number-sequence ?1 ?9)
                        '(?0)))

  (setq ajb-sort-function 'bs--sort-by-recentf)
  )

(provide 'ace-config)
