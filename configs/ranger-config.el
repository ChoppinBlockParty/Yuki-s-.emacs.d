(use-package ranger
  :ensure t
  :config
  (progn
    ; (defun my-configure-sinrise-commander ()
    ;   (sr-omit-mode 1))
    ; (add-hook 'dired-mode-hook 'my-configure-sinrise-commander)
    (define-key evil-normal-state-map "`" 'ranger)
    (setq ranger-parent-depth 2)
    (setq ranger-show-dotfiles t)
    (setq ranger-width-preview 0.55)
    (setq ranger-ignored-extensions '("mkv" "iso" "mp4"))
    (setq ranger-max-preview-size 10)
    )
)

(after 'evil
)

(provide 'ranger-config)
