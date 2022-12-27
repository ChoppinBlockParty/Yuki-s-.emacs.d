;;; Looks fancy, unfortunately, does not work very well probably
;;; because the project is too complicated.

(use-package treemacs
  :config
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))

    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-read-string-input             'from-child-frame
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         35
          treemacs-workspace-switch-cleanup      nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

  (define-key (current-global-map) (kbd "M-0") 'treemacs-select-window)
  )

(use-package treemacs-evil
  :after (treemacs evil-config)
  :config

  (my-evil-2-modes-define-key "TAB" 'treemacs-display-current-project-exclusively)
  (my-evil-2-modes-define-key "<tab>" 'treemacs-display-current-project-exclusively)
  (my-evil-2-modes-define-key "SPC u t" 'treemacs)
  (my-evil-2-modes-define-key "SPC u p t" 'treemacs-projectile)

  (define-key evil-treemacs-state-map (kbd "SPC l") #'evil-window-right)
  (define-key evil-treemacs-state-map (kbd "TAB") #'treemacs-quit)
  (define-key evil-treemacs-state-map (kbd "<tab>") #'treemacs-quit)
  (define-key evil-treemacs-state-map (kbd "o") #'treemacs-TAB-action)

  (evil-define-key 'treemacs treemacs-mode-map
    (kbd "SPC 0") 'select-window-0
    (kbd "SPC 1") 'select-window-1
    (kbd "SPC 2") 'select-window-2
    (kbd "SPC 3") 'select-window-3
    (kbd "SPC 4") 'select-window-4
    (kbd "SPC 5") 'select-window-5
    (kbd "SPC 6") 'select-window-6
    (kbd "SPC 7") 'select-window-7
    (kbd "SPC 8") 'select-window-8
    (kbd "SPC 9") 'select-window-9)
  )

(use-package treemacs-projectile
  :after (treemacs projectile)
  )

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :config (treemacs-set-scope-type 'Perspectives))

(provide 'treemacs-config)
;;; treemacs-config.el ends here
