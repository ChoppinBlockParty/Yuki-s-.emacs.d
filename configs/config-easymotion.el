;; https://github.com/abo-abo/avy/wiki/defcustom

(after 'evil
(use-package evil-easymotion
  :ensure t
  :config
  (progn
    (setq avy-keys (number-sequence ?a ?z))

    (evilem-define "J" 'evil-backward-word-begin)
    (evilem-define "K" 'evil-forward-word-begin)
    (evilem-define "H" 'evil-backward-word-end)
    (evilem-define "L" 'evil-forward-word-end)

    (evilem-define "F" 'evil-repeat-find-char
      (lambda ()
        (save-excursion
          (let ((evil-cross-lines t))
            (call-interactively 'evil-find-char-to))))
      nil
      ((evil-cross-lines t)))

    (evilem-define "S" 'evil-repeat-find-char
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

    (evilem-define "s" 'evil-repeat-find-char
      (lambda ()
        (save-excursion
          (let ((evil-cross-lines t))
            (call-interactively 'evil-find-char-backward))))
      nil
      ((evil-cross-lines t)))
  )
)
)

(provide 'config-easymotion)
