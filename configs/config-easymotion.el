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
    )
)
)

(provide 'config-easymotion)
