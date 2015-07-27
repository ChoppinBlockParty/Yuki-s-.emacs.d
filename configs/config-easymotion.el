(use-package evil-easymotion
  :ensure t
  :config
  (progn
    ; (define-key evil-normal-state-map "H" 'ace-jump-mode)
    (evilem-define "H" 'evil-backward-word-begin)
    (evilem-define "L" 'evil-forward-word-begin)

    (evilem-define "t" 'evil-repeat-find-char
      (lambda ()
        (save-excursion
          (let ((evil-cross-lines t))
            (call-interactively 'evil-find-char-to))))
      nil
      ((evil-cross-lines t)))

    (evilem-define "T" 'evil-repeat-find-char
      (lambda ()
        (save-excursion
          (let ((evil-cross-lines t))
            (call-interactively 'evil-find-char-to-backward))))
      nil
      ((evil-cross-lines t)))

    (evilem-define "f" 'evil-repeat-find-char
      (lambda ()
        (save-excursion
          (let ((evil-cross-lines t))
            (call-interactively 'evil-find-char))))
      nil
      ((evil-cross-lines t)))

    (evilem-define "F" 'evil-repeat-find-char
      (lambda ()
        (save-excursion
          (let ((evil-cross-lines t))
            (call-interactively 'evil-find-char-backward))))
      nil
      ((evil-cross-lines t)))
  )
)

(provide 'config-easymotion)
