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


    (after 'evil
      (progn
      (defhydra helm-like-unite (:hint nil
                                 :color pink)
        ;; arrows
        ("h" helm-beginning-of-buffer)
        ("j" helm-next-line)
        ("k" helm-previous-line)
        ("l" helm-end-of-buffer)
        ;; beginning/end
        ("g" helm-beginning-of-buffer)
        ("G" helm-end-of-buffer)
        ;; scroll
        ("K" helm-scroll-other-window-down)
        ("J" helm-scroll-other-window)
        ;; mark
        ("v" helm-toggle-visible-mark)
        ("V" helm-unmark-all)
        ("U" helm-toggle-all-marks)
        ;; exit
        ("<escape>" keyboard-escape-quit "" :exit t)
        ("o" keyboard-escape-quit :exit t)
        ("i" nil)
        ;; sources
        ("}" helm-next-source)
        ("{" helm-previous-source)
        ;; rest
        ("H" helm-help)
        ("p" helm-execute-persistent-action)
        ("d" helm-persistent-delete-marked)
        ("f" helm-follow-mode))
      
      
      (defun helm-persistent-delete-marked ()
        "Kill buffer without quitting helm."
        (interactive)
        (if (equal (cdr (assoc 'name (helm-get-current-source)))
                   "Buffers")
            (with-helm-alive-p
              (helm-attrset 'kill-action
                            '(helm-persistent-kill-buffers . never-split))
              (helm-execute-persistent-action 'kill-action))
          (user-error "Only works for buffers")))
      (defun helm-persistent-kill-buffers (_buffer)
        (unwind-protect
             (dolist (b (helm-marked-candidates))
               (helm-buffers-persistent-kill-1 b))
          (with-helm-buffer
            (setq helm-marked-candidates nil
                  helm-visible-mark-overlays nil))
          (helm-force-update (helm-buffers--quote-truncated-buffer
                              (helm-get-selection)))))

      ; (define-key helm-map (kbd "<escape>") 'helm-like-unite/body)

      (define-key evil-normal-state-map "\M-w" 'helm-buffers-list)
      (define-key evil-normal-state-map "\M-s" 'helm-buffers-list)
      (define-key evil-normal-state-map "\M-e" 'helm-for-files)
      (define-key evil-normal-state-map "\M-d" 'helm-for-files)
      ))

; (define-key evil-normal-state-map "\M-w" 'helm-buffers-list)
; (define-key evil-normal-state-map "\M-s" 'helm-buffers-list)
; (define-key evil-normal-state-map "\M-e" 'helm-for-files)
; (define-key evil-normal-state-map "\M-d" 'helm-for-files)


    (after 'evil-leader
        (evil-leader/set-key "b" 'helm-mini)
        (evil-leader/set-key "i" 'helm-imenu))


    (after 'flycheck
      (use-package helm-flycheck
        :ensure helm-flycheck)))

(provide 'configs-helm)
