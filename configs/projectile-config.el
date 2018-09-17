
(use-package projectile
  :ensure t
  :config
  (progn
    (projectile-global-mode t)
    (setq
      projectile-indexing-method 'alien
      projectile-enable-caching t
      projectile-cache-file (concat user-emacs-directory ".cache/projectile.cache")
      projectile-known-projects-file (concat user-emacs-directory "projectile-bookmarks.eld")
      )
    ;; automatically dired in projectile-switch-project
    (setq projectile-switch-project-action 'projectile-dired)
    (setq projectile-completion-system 'ivy)
    (add-to-list 'projectile-globally-ignored-directories "~")
    (setq projectile-globally-ignored-files '(
      "/home/smikerov/Makefile"
      ".tags"
      "tags"
      ".tag"
      "tag"
      ))
    ))

(provide 'projectile-config)
