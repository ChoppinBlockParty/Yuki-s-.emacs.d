;;; easymotion-config --- Summary
;;; Commentary:
;;; Code:
(use-package avy
  :config
  (setq
    ;;; When non-nil, a gray background will be added during the selection.
    avy-background t
    ;;; at: - single character path on target, obscuring the target.
    avy-style 'at
    ;;; Any lower-case letter or number.  Numbers are specified in the keyboard
    ;;; number-row order, so that the candidate following '9' will be '0'.
    avy-keys (nconc (number-sequence ?a ?z))
    avy-word-punc-regexp nil
    ;;; In case there is only one candidate jumps directly to it."
    avy-single-candidate-jump nil
    )
  (custom-set-faces
    '(avy-lead-face ((t (:background nil :foreground "#2192FF" :weight bold))))
    ;; '(avy-lead-face-0 ((t (:background nil :foreground ,black-4))))
    ;; '(avy-lead-face-1 ((t (:background nil :foreground ,black-4))))
    ;; '(avy-lead-face-2 ((t (:background nil :foreground ,black-4))))
    )
  )

;;; evil-easymotion depends on avy
(use-package evil-easymotion
  :config
  )

(provide 'easymotion-config)
;;; easymotion-config.el ends here
