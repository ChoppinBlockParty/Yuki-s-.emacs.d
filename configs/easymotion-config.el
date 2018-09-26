;;; easymotion-config --- Summary
;;; Commentary:
;;; Code:
(use-package evil-easymotion
  :ensure t
  :config
  ;; When non-nil, a gray background will be added during the selection.
  (setq avy-background t)
  ;; at: - single character path on target, obscuring the target.
  (setq avy-style 'at)
  ;; Any lower-case letter or number.  Numbers are specified in the keyboard
  ;; number-row order, so that the candidate following '9' will be '0'.
  (setq avy-keys (nconc (number-sequence ?a ?z)))
  (setq avy-word-punc-regexp nil)
  (custom-set-faces
    '(avy-lead-face ((t (:background nil :foreground "#ff2929" :weight bold))))
    ;; '(avy-lead-face-0 ((t (:background nil :foreground ,black-4))))
    ;; '(avy-lead-face-1 ((t (:background nil :foreground ,black-4))))
    ;; '(avy-lead-face-2 ((t (:background nil :foreground ,black-4))))
    )
  )

(provide 'easymotion-config)
;;; easymotion-config.el ends here
