(use-package hydra
  :ensure t
  :demand
)

(require 'helm-files)

(after 'projectile
(use-package helm-projectile
  :ensure t)
(after 'evil
(use-package helm
  :ensure helm
  :config
  (progn

    (use-package helm-config
      :config
      (progn))
    (use-package helm-themes
      :ensure t
      :config
      (progn))

    (setq helm-split-window-in-side-p      t ; open helm buffer inside current window, not occupy whole other window
     ;; helm-split-window-default-side        'above
     helm-echo-input-in-header-line        t
     ;; helm-display-header-line              nil ; t by default
     helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
     helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
     helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
     helm-ff-file-name-history-use-recentf t
     ;; helm-autoresize-mode                  t
     helm-M-x-fuzzy-match                  t ; optional fuzzy matching for helm-M-x
     helm-buffers-fuzzy-matching           t
     helm-recentf-fuzzy-match              t
     helm-semantic-fuzzy-match             t
     helm-imenu-fuzzy-match                t
     )

    (defun helm-hide-minibuffer-maybe ()
        (when (with-helm-buffer helm-echo-input-in-header-line)
            (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
            (overlay-put ov 'window (selected-window))
            (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                                    `(:background ,bg-color :foreground ,bg-color)))
            (setq-local cursor-type nil))))

    (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)

    (defvar helm-source-header-default-background (face-attribute 'helm-source-header :background))
    (defvar helm-source-header-default-foreground (face-attribute 'helm-source-header :foreground))
    (defvar helm-source-header-default-box (face-attribute 'helm-source-header :box))

    (defun helm-toggle-header-line ()
        (if (> (length helm-sources) 1)
            (set-face-attribute 'helm-source-header
                                nil
                                :foreground helm-source-header-default-foreground
                                :background helm-source-header-default-background
                                :box helm-source-header-default-box
                                :height 0.9)
            (set-face-attribute 'helm-source-header
                                nil
                                :foreground (face-attribute 'helm-selection :background)
                                :background (face-attribute 'helm-selection :background)
                                :box nil
                                :height 0.1)))

    (add-hook 'helm-before-initialize-hook 'helm-toggle-header-line)

    (defun my-helm-in-ido (buffer)
      "Display a helm buffer in ido. Send the purists screaming."
      (interactive)
      (ido-buffer-internal 'display 'display-buffer nil nil nil 'ignore)))

    (setq helm-display-function 'helm-default-display-buffer)
    (setq helm-adaptive-history-file "~/.emacs.d/helm-adapative-history")

    (global-set-key (kbd "M-x") 'helm-M-x)
    ;; (define-key evil-motion-state-map (kbd ":") 'execute-extended-command)
    (define-key evil-motion-state-map (kbd ":") 'helm-M-x)

    ;; shell history.
    (define-key shell-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)

    ;; use helm to list eshell history
    (add-hook 'eshell-mode-hook
            #'(lambda ()
                (substitute-key-definition 'eshell-list-history 'helm-eshell-history eshell-mode-map)))

    (substitute-key-definition 'find-tag 'helm-etags-select global-map)
    (setq projectile-completion-system 'helm)
    (helm-mode 1)

    ;; enable Helm version of Projectile with replacment commands
    (helm-projectile-on)


      ;; helm navigation on hjkl
      (defun spacemacs//helm-hjkl-navigation (&optional arg)
        "Set navigation in helm on `jklh'.
ARG non nil means that the editing style is `vim'."
        (cond
         (arg
            (define-key helm-map (kbd "C-j") 'helm-next-line)
            (define-key helm-map (kbd "C-k") 'helm-previous-line)
            (define-key helm-map (kbd "C-h") 'helm-next-source)
            (define-key helm-map (kbd "C-l") 'helm-previous-source))
         (t
            (define-key helm-map (kbd "C-j") 'helm-execute-persistent-action)
            (define-key helm-map (kbd "C-k") 'helm-delete-minibuffer-contents)
            (define-key helm-map (kbd "C-h") nil)
            (define-key helm-map (kbd "C-l") 'helm-recenter-top-bottom-other-window))))

       (spacemacs//helm-hjkl-navigation (eq 'vim dotspacemacs-editing-style))

      (defun spacemacs/helm-edit ()
        "Switch in edit mode depending on the current helm buffer."
        (interactive)
        (cond
         ((string-equal "*helm-ag*" helm-buffer)
          (helm-ag-edit))))

      (defun spacemacs//helm-navigation-ms-on-enter ()
        "Initialization of helm micro-state."
        ;; faces
        (spacemacs//helm-navigation-ms-set-face)
        (setq spacemacs--helm-navigation-ms-face-cookie-minibuffer
              (face-remap-add-relative
               'minibuffer-prompt
               'spacemacs-helm-navigation-ms-face))
        ;; bind actions on numbers starting from 1 which executes action 0
        (dotimes (n 10)
          (define-key helm-map (number-to-string n)
            `(lambda () (interactive) (helm-select-nth-action
                                       ,(% (+ n 9) 10))))))

      (defun spacemacs//helm-navigation-ms-set-face ()
        "Set the face for helm header in helm navigation micro-state"
        (with-helm-window
          (setq spacemacs--helm-navigation-ms-face-cookie-header
                (face-remap-add-relative
                 'helm-header
                 'spacemacs-helm-navigation-ms-face))))

      (defun spacemacs//helm-navigation-ms-on-exit ()
        "Action to perform when exiting helm micro-state."
        ;; restore helm key map
        (dotimes (n 10) (define-key helm-map (number-to-string n) nil))
        ;; restore faces
        (with-helm-window
          (face-remap-remove-relative
           spacemacs--helm-navigation-ms-face-cookie-header))
        (face-remap-remove-relative
         spacemacs--helm-navigation-ms-face-cookie-minibuffer))

      (defun spacemacs//helm-navigation-ms-full-doc ()
        "Full documentation for helm navigation micro-state."
        "
  [?]          display this help
  [a]          toggle action selection page
  [e]          edit occurrences if supported
  [j] [k]      next/previous candidate
  [h] [l]      previous/next source
  [t]          toggle visible mark
  [T]          toggle all mark
  [v]          persistent action
  [q]          quit")

      (spacemacs|define-micro-state helm-navigation
        :persistent t
        :disable-evil-leader t
        :define-key (helm-map . "<escape>") (helm-map . "ESC")
        :on-enter (spacemacs//helm-navigation-ms-on-enter)
        :on-exit  (spacemacs//helm-navigation-ms-on-exit)
        :bindings
        ("<tab>" helm-select-action)
        ("C-i" helm-select-action :exit t)
        ("<RET>" helm-maybe-exit-minibuffer :exit t)
        ("?" nil :doc (spacemacs//helm-navigation-ms-full-doc))
        ("a" helm-select-action :post (spacemacs//helm-navigation-ms-set-face))
        ("e" spacemacs/helm-edit)
        ("h" helm-previous-source)
        ("j" helm-next-line)
        ("k" helm-previous-line)
        ("l" helm-next-source)
        ("g" helm-beginning-of-buffer)
        ("G" helm-end-of-buffer)
        ("C-q" keyboard-escape-quit :exit t)
        ("C-u" keyboard-escape-quit :exit t)
        ("<escape>" keyboard-escape-quit :exit t)
        ("q" nil :exit t)
        ("i" nil :exit t)
        ("v" helm-toggle-visible-mark)
        ("u" helm-toggle-all-marks)
        ("p" helm-execute-persistent-action)
        )

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
      :ensure t
      :config
      (progn
        ;; Don't start searching for the thing at point by default.
        ;; Let me type it.
        (setq helm-swoop-pre-input-function (lambda () ()))
        (define-key evil-normal-state-map (kbd "C-f")   'helm-swoop)
        (define-key evil-visual-state-map (kbd "C-f")   'helm-swoop)
        )
     )


    (after 'flycheck
      (use-package helm-flycheck
        :ensure helm-flycheck))

    (defhydra helm-like-unite (:hint nil
                               :pre (set-cursor-color "#e52b50")
                               :post (set-cursor-color "#ffffff")
                               ;; :color amaranth
                               )
      ("?" helm-help)
      ("<escape>" keyboard-escape-quit)
      ("<SPC>" helm-toggle-visible-mark "mark")
      ("v" helm-toggle-visible-mark "mark")
      ("a" helm-toggle-all-marks "(un)mark all")
      ;; not sure if there's a better way to do this
      ("/" (lambda ()
              (interactive)
              (execute-kbd-macro [?\C-s]))
           "search")
      ;; ("v" helm-execute-persistent-action)
      ;; suggested by Sylvain Benner (syl20bnr)
      ;; if you want to use a key besides TAB to go to action select and then exit the hydra
      ("r" helm-select-action :color blue)
      )

    ;; (define-key helm-map (kbd "<escape>") 'helm-like-unite/body)
    (define-key helm-map (kbd "C-q") 'keyboard-escape-quit)
    (define-key helm-map (kbd "C-u") 'keyboard-escape-quit)

    (define-key evil-normal-state-map "\M-w" 'helm-mini)
    (define-key evil-normal-state-map "\M-d" 'helm-for-files)
    (define-key evil-normal-state-map "\M-s" 'helm-mini)
    (define-key evil-normal-state-map "\M-e" 'helm-for-files)

    (define-key evil-normal-state-map (kbd "SPC m m") 'helm-mini)
    (define-key evil-normal-state-map (kbd "SPC m f") 'helm-find)
    (define-key evil-normal-state-map (kbd "SPC m i") 'helm-semantic-or-imenu)
    (define-key evil-normal-state-map (kbd "?") 'helm-do-ag-this-file)
    (define-key evil-visual-state-map (kbd "?") 'helm-do-ag-this-file)

    (use-package helm-ag
      :ensure t
      :demand
      :config
      (progn
        )
     )

    (evil-leader/set-key
       "q" 'find-file
       "w" 'helm-for-files
       ;; "l" 'helm-locate
       ;; "y" 'helm-show-kill-ring
       ;; "t" 'helm-top
       ;; "m" 'helm-man-woman
       ;; "o" 'helm-occur
       ;; "j" 'helm-M-x
       "e" 'helm-find-files
       "b" 'helm-buffers-list
       "f" 'helm-projectile-find-file
       "r" 'helm-recentf
       "A" 'helm-projectile
       "a" 'helm-projectile-ag)
        ;; (eval-after-load "helm-ag"
        ;;     (evil-leader/set-key
        ;;     "a" 'helm-projectile-ag))
        ;; (eval-after-load "expand-region"
        ;;     (progn
        ;;     (setq expand-region-contract-fast-key "z")
        ;;     (evil-leader/set-key "xx" 'er/expand-region)))
        ;; (eval-after-load "magit"
        ;;     (evil-leader/set-key "g" 'magit-status))
        ;; (eval-after-load "quickrun"
        ;;     (evil-leader/set-key "q" 'quickrun))
        ;; (eval-after-load "git-gutter-mode"
        ;;     (evil-leader/set-key
        ;;     "s" 'git-gutter:stage-hunk
        ;;     "n" 'git-gutter:next-hunk
        ;;     "p" 'git-gutter:previous-hunk))
        ;; (evil-leader/set-key
        ;;     "k" 'kill-buffer
        ;;     "d" 'dired
        ;;     "z" 'repeat
        ;;     "0" 'delete-window
        ;;     "1" 'delete-other-windows
        ;;     "2" 'split-window-below
        ;;     "3" 'split-window-right)
        ;; (global-evil-leader-mode))
)
)
)

(provide 'configs-helm)
