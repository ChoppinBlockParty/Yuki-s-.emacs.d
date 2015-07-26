(use-package hydra
  :ensure t
  :demand
)

(use-package helm
  :ensure helm
  :config
  (progn
    (use-package helm-config
      :config
      (progn))
    (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
          helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
          helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
          helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
          helm-ff-file-name-history-use-recentf t
          helm-buffers-fuzzy-matching           t)
    (defun my-helm-in-ido (buffer)
      "Display a helm buffer in ido. Send the purists screaming."
      (interactive)
      (ido-buffer-internal 'display 'display-buffer nil nil nil 'ignore)))
    (setq helm-display-function 'helm-default-display-buffer)
    (setq helm-adaptive-history-file "~/.emacs.d/helm-adapative-history")

    (define-key helm-map (kbd "C-p") 'helm-execute-persistent-action)
    (define-key helm-map (kbd "C-n") 'helm-delete-minibuffer-contents)
    (define-key helm-map (kbd "C-j") 'helm-next-line)
    (define-key helm-map (kbd "C-k") 'helm-previous-line)

    (require 'helm-files)

    (after 'projectile
      (use-package helm-projectile
        :ensure helm-projectile))


    (defun helm-jump ()
      "Find files with helm, but be smart about buffers and recent files."
      (interactive)
      (let ((helm-ff-transformer-show-only-basename nil))
        (helm-other-buffer '(helm-projectile-sources-list
                             helm-source-buffers-list
                             helm-source-recentf
                             helm-source-bookmarks
                             helm-source-file-cache
                             helm-source-files-in-current-dir
                             helm-source-locate
                             helm-source-buffer-not-found)
                           "*helm jump*")))

    (setq helm-command-prefix-key "C-c h")
    (setq helm-quick-update t)

    (use-package helm-swoop
      :ensure helm-swoop
      :config
      (progn
        ;; Don't start searching for the thing at point by default.
        ;; Let me type it.
        (setq helm-swoop-pre-input-function (lambda () ()))
        (after 'evil
          (define-key evil-normal-state-map (kbd "SPC l")   'helm-swoop))))

      (define-key evil-normal-state-map "\M-s" 'helm-buffers-list)
      (define-key evil-normal-state-map "\M-e" 'helm-for-files)

    (after 'evil-leader
        (evil-leader/set-key "b" 'helm-mini)
        (evil-leader/set-key "i" 'helm-imenu))


    (after 'flycheck
      (use-package helm-flycheck
        :ensure helm-flycheck))

    (defhydra helm-like-unite (:hint nil)
      "vim movement"
      ("?" helm-help "help")
      ("<escape>" keyboard-escape-quit "exit")
      ("<SPC>" helm-toggle-visible-mark "mark")
      ("a" helm-toggle-all-marks "(un)mark all")
      ;; not sure if there's a better way to do this
      ("/" (lambda ()
              (interactive)
              (execute-kbd-macro [?\C-s]))
           "search")
      ("v" helm-execute-persistent-action)
      ("g" helm-beginning-of-buffer "top")
      ("G" helm-end-of-buffer "bottom")
      ("j" helm-next-line "down")
      ("k" helm-previous-line "up")
      ("i" nil "cancel"))

    (defun my-helm-like-unite-enter ()
      (interactive)
      (helm-buffers-list)
      (helm-like-unite/body)
    )
    
    (define-key evil-normal-state-map "\M-w" 'my-helm-like-unite-enter)

    (define-key helm-map (kbd "<escape>") 'my-helm-like-unite-enter)
)

(provide 'configs-helm)
