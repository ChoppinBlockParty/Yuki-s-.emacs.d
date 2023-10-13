;;; global-key-binding-config --- My global bindings
;;; Commentary:
;;; Code:

;;; Disable NEWS binding, always accidentally hit it
(global-unset-key (kbd "C-h n"))
(global-unset-key (kbd "C-h C-n"))

(evil-define-key 'normal 'global (kbd "C-r") 'my-vterm-run-last-command-in-last-shell)
(evil-define-key 'visual 'global (kbd "C-r") 'my-vterm-run-last-command-in-last-shell)
(evil-define-key 'insert 'global (kbd "C-r") 'my-vterm-run-last-command-in-last-shell)

;; (my-evil-all-modes-define-key (kbd "C-z") 'evil-show-registers)
(evil-define-key 'normal 'global (kbd "C-z") 'evil-show-registers)
(evil-define-key 'visual 'global (kbd "C-z") 'evil-show-registers)
(evil-define-key 'insert 'global (kbd "C-z") 'evil-show-registers)

(evil-define-key 'motion 'global "gd" 'dired-jump)

;;; If you want to correctly redefine tab need to use as in example below,
;;; and not <tab> or anything else, otherwise it won't be overridden by other modes.
;; (evil-define-key 'normal 'global (kbd "TAB") 'evil-show-registers)

(my-evil-2-modes-define-key "SPC u ?"     'counsel-apropos)
(my-evil-2-modes-define-key "SPC i r"     'ivy-resume)
(my-evil-2-modes-define-key "SPC u i c"   'counsel-colors-web)
(my-evil-2-modes-define-key "SPC u i g a" 'counsel-ag)
(my-evil-2-modes-define-key "SPC u i g g" 'counsel-git-grep)
(my-evil-2-modes-define-key "SPC u i g r" 'counsel-rg)
(my-evil-2-modes-define-key "SPC u i l"   'counsel-locate)
(my-evil-2-modes-define-key "SPC u i m"   'man)
(my-evil-2-modes-define-key "SPC u i p"   'counsel-list-processes)
(my-evil-2-modes-define-key "SPC u i r"   'counsel-recentf)
(my-evil-2-modes-define-key "SPC u i u"   'counsel-unicode-char)
(my-evil-2-modes-define-key "SPC u i s"   'counsel-info-lookup-symbol)

(my-evil-2-modes-define-key "SPC u f d"   'dired)

(my-evil-2-modes-define-key "SPC u g"     'magit-status)
(my-evil-2-modes-define-key "SPC u m b"   'magit-blame)
(my-evil-2-modes-define-key "SPC u m d c" 'magit-diff-buffer-file-popup)
(my-evil-2-modes-define-key "SPC u m d a" 'magit-diff-dwim)
(my-evil-2-modes-define-key "SPC u m f"   'magit-fetch-all)
(my-evil-2-modes-define-key "SPC u m f"   'magit-fetch-all)
(my-evil-2-modes-define-key "SPC u m l c" 'magit-log-buffer-file-popup)
(my-evil-2-modes-define-key "SPC u m l a" 'magit-log-all)
(my-evil-2-modes-define-key "SPC u m p"   'magit-pull)
(my-evil-2-modes-define-key "SPC u m s"   'magit-stash)

(my-evil-2-modes-define-key "SPC u p d"   'counsel-projectile-find-dir)
(my-evil-2-modes-define-key "SPC u p g a" 'counsel-projectile-ag)
(my-evil-2-modes-define-key "SPC u p g r" 'counsel-projectile-rg)
(my-evil-2-modes-define-key "SPC u p r"   'projectile-recentf)
(my-evil-2-modes-define-key "SPC u p s"   'counsel-projectile-switch-project)

(my-evil-2-modes-define-key "SPC u r s"   'rg)
(my-evil-2-modes-define-key "SPC u r r"   'rg-search-everything-in-project)
(my-evil-2-modes-define-key "SPC u r d"   'rg-dwim)
(my-evil-2-modes-define-key "SPC u r l"   'rg-literal)

;; (my-evil-2-modes-define-key "SPC u s"     'my-shell)
(my-evil-2-modes-define-key "SPC u s"     'my-vterm)

(my-evil-2-modes-define-key "SPC u y ."   'ycmd-goto)
(my-evil-2-modes-define-key "SPC u y ?"   'ycmd-show-documentation)
(my-evil-2-modes-define-key "SPC u y c"   'ycmd-close)
(my-evil-2-modes-define-key "SPC u y C"   'ycmd-clear-compilation-flag-cache)
(my-evil-2-modes-define-key "SPC u y e"   'ycmd-goto-references)
(my-evil-2-modes-define-key "SPC u y f"   'ycmd-fixit)
(my-evil-2-modes-define-key "SPC u y i"   'ycmd-goto-include)
(my-evil-2-modes-define-key "SPC u y l"   'ycmd-goto-implementation)
(my-evil-2-modes-define-key "SPC u y m"   'ycmd-goto-imprecise)
(my-evil-2-modes-define-key "SPC u y o"   'ycmd-open)
(my-evil-2-modes-define-key "SPC u y O"   'ycmd-restart-semantic-server)
(my-evil-2-modes-define-key "SPC u y r"   'ycmd-refactor-rename)
(my-evil-2-modes-define-key "SPC u y p"   'ycmd-parse-buffer)
(my-evil-2-modes-define-key "SPC u y s"   'ycmd-toggle-force-semantic-completion)
(my-evil-2-modes-define-key "SPC u y t"   'ycmd-goto-type)
(my-evil-2-modes-define-key "SPC u y T"   'ycmd-get-parent)
(my-evil-2-modes-define-key "SPC u y x"   'ycmd-completer)
(my-evil-2-modes-define-key "SPC u y y"   'ycmd-get-type)

(provide 'global-key-binding-config)
;;; global-key-binding-config.el ends here
