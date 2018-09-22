(use-package projectile
  :after (counsel)
  :config
  (setq
    projectile-mode-line nil
   ;; automatically dired in projectile-switch-project
    projectile-switch-project-action 'projectile-dired
    projectile-completion-system 'ivy
    projectile-indexing-method 'alien
    projectile-enable-caching t
    projectile-cache-file "~/.cache/emacs/projectile-cache"
    projectile-known-projects-file "~/.cache/emacs/projectile-bookmarks"
    projectile-globally-ignored-files '(
      ".tags"
      "tags"
      ".tag"
      "tag"
      )
    )
  (projectile-global-mode t)
  )

(provide 'projectile-config)
