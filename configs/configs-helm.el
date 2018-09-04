(use-package helm-files)

(after 'projectile
(use-package helm-projectile
  :ensure t)

(after 'evil
(use-package helm
  :ensure t
  :config
  (progn

    (use-package helm-config
      :config
      (progn))


    (use-package helm-themes
      :ensure t
      :config
      (progn))

    ;; helm-find-files: one command that handles all the files related commands (bind to C-x C-f).
    ;; helm-buffers-list: provides enhanced buffers listing.
    ;; helm-browse-project: handles project files and buffers; defaults to current directory; works with helm-find-files; recommended with helm-ls-git, helm-ls-hg and helm-ls-svn for a better handling of version control files. Each time a project under version control is visited it is added to helm-browse-project-history and can be visted with helm-projects-history.
    ;; helm-dabbrev: enhanced dabbrev implementation with helm completion; does not use emacs code.
    ;; helm-moccur: enhanced occur for one or more buffers; launch from helm-buffers-list or current-buffer.
    ;; helm-M-x: enhanced execute-extended-command (bind it to M-x).
    ;; helm-imenu and helm-imenu-in-all-buffers: provide imenus for current or all buffers.
    ;; helm-etags-select: enhanced etags with helm-completion; usable everywhere with helm-find-files.
    ;; helm-apropos: enhanced apropos for functions and variables that C-h commands provide.
    ;; Grep: launch from any helm file commands; supports back-ends grep, ack-grep, git-grep, ag and custom implementation of pt.
    ;; helm-gid: Helm interface for gid from id-utils.
    ;; helm-show-kill-ring: A helm browser for kill ring.
    ;; helm-all-mark-rings: A helm browser for mark ring; retrieves last positions in buffers.
    ;; helm-filtered-bookmarks: enhanced browser for bookmarks.
    ;; helm-list-elisp-packages: enhanced browser for elisp package management.

    ;; helm-mode: turns on helm completions for most standard emacs completions. Helm provides even more optimized helm completions for some commands in helm-mode. Prefer these natively optimized versions over the ones in helm-mode.
    (helm-mode t)

    (setq
      ; open helm buffer inside current window, not occupy whole other window
      helm-split-window-inside-p           t
      helm-split-window-default-side        'above
      ; If not set completion opens a new frame/window
      helm-show-completion-display-function #'helm-show-completion-default-display-function
      helm-echo-input-in-header-line        t
      helm-display-header-line              t ; t by default
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-autoresize-mode                  t
      helm-ff-skip-boring-files             t
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

    (defun my-helm-in-ido (buffer)
      "Display a helm buffer in ido. Send the purists screaming."
      (interactive)
      (ido-buffer-internal 'display 'display-buffer nil nil nil 'ignore)))

    (setq helm-display-function 'helm-default-display-buffer)
    (setq helm-adaptive-history-file "~/.emacs.d/.helm-adapative-history")

    (global-set-key (kbd "M-x") 'helm-M-x)

    ;; shell history.
    (define-key shell-mode-map (kbd "C-r") 'helm-comint-input-ring)

    ;; use helm to list eshell history
    (add-hook 'eshell-mode-hook
            #'(lambda ()
                (substitute-key-definition 'eshell-list-history 'helm-eshell-history eshell-mode-map)))

    (substitute-key-definition 'find-tag 'helm-etags-select global-map)
    (setq projectile-completion-system 'helm)

    ;; enable Helm version of Projectile with replacment commands
    (helm-projectile-on)

;; Helm like Unite
;;       ;; helm navigation on hjkl
;;       (defun spacemacs//helm-hjkl-navigation (&optional arg)
;;         "Set navigation in helm on `jklh'.
;; ARG non nil means that the editing style is `vim'."
;;         (cond
;;          (arg
;;             (define-key helm-map (kbd "C-j") 'helm-next-line)
;;             (define-key helm-map (kbd "C-k") 'helm-previous-line)
;;             (define-key helm-map (kbd "C-h") 'helm-next-source)
;;             (define-key helm-map (kbd "C-l") 'helm-previous-source))
;;          (t
;;             (define-key helm-map (kbd "C-j") 'helm-execute-persistent-action)
;;             (define-key helm-map (kbd "C-k") 'helm-delete-minibuffer-contents)
;;             (define-key helm-map (kbd "C-h") nil)
;;             (define-key helm-map (kbd "C-l") 'helm-recenter-top-bottom-other-window))))

;;        (spacemacs//helm-hjkl-navigation (eq 'vim dotspacemacs-editing-style))

;;       (defun spacemacs/helm-edit ()
;;         "Switch in edit mode depending on the current helm buffer."
;;         (interactive)
;;         (cond
;;          ((string-equal "*helm-ag*" helm-buffer)
;;           (helm-ag-edit))))

;;       (defun spacemacs//helm-navigation-ms-on-enter ()
;;         "Initialization of helm micro-state."
;;         ;; faces
;;         (spacemacs//helm-navigation-ms-set-face)
;;         (setq spacemacs--helm-navigation-ms-face-cookie-minibuffer
;;               (face-remap-add-relative
;;                'minibuffer-prompt
;;                'spacemacs-helm-navigation-ms-face))
;;         ;; bind actions on numbers starting from 1 which executes action 0
;;         (dotimes (n 10)
;;           (define-key helm-map (number-to-string n)
;;             `(lambda () (interactive) (helm-select-nth-action
;;                                        ,(% (+ n 9) 10))))))

;;       (defun spacemacs//helm-navigation-ms-set-face ()
;;         "Set the face for helm header in helm navigation micro-state"
;;         (with-helm-window
;;           (setq spacemacs--helm-navigation-ms-face-cookie-header
;;                 (face-remap-add-relative
;;                  'helm-header
;;                  'spacemacs-helm-navigation-ms-face))))

;;       (defun spacemacs//helm-navigation-ms-on-exit ()
;;         "Action to perform when exiting helm micro-state."
;;         ;; restore helm key map
;;         (dotimes (n 10) (define-key helm-map (number-to-string n) nil))
;;         ;; restore faces
;;         (with-helm-window
;;           (face-remap-remove-relative
;;            spacemacs--helm-navigation-ms-face-cookie-header))
;;         (face-remap-remove-relative
;;          spacemacs--helm-navigation-ms-face-cookie-minibuffer))

;;       (defun spacemacs//helm-navigation-ms-full-doc ()
;;         "Full documentation for helm navigation micro-state."
;;         "
;;   [?]          display this help
;;   [a]          toggle action selection page
;;   [e]          edit occurrences if supported
;;   [j] [k]      next/previous candidate
;;   [h] [l]      previous/next source
;;   [t]          toggle visible mark
;;   [T]          toggle all mark
;;   [v]          persistent action
;;   [q]          quit")

;;       (spacemacs|define-micro-state helm-navigation
;;         :persistent t
;;         :disable-evil-leader t
;;         :define-key (helm-map . "<escape>") (helm-map . "ESC")
;;         :on-enter (spacemacs//helm-navigation-ms-on-enter)
;;         :on-exit  (spacemacs//helm-navigation-ms-on-exit)
;;         :bindings
;;         ("<tab>" helm-select-action)
;;         ("C-i" helm-select-action :exit t)
;;         ("<RET>" helm-maybe-exit-minibuffer :exit t)
;;         ("?" nil :doc (spacemacs//helm-navigation-ms-full-doc))
;;         ("a" helm-select-action :post (spacemacs//helm-navigation-ms-set-face))
;;         ("e" spacemacs/helm-edit)
;;         ("h" helm-previous-source)
;;         ("j" helm-next-line)
;;         ("k" helm-previous-line)
;;         ("l" helm-next-source)
;;         ("g" helm-beginning-of-buffer)
;;         ("G" helm-end-of-buffer)
;;         ("C-q" keyboard-escape-quit :exit t)
;;         ("C-u" keyboard-escape-quit :exit t)
;;         ("<escape>" keyboard-escape-quit :exit t)
;;         ("q" nil :exit t)
;;         ("i" nil :exit t)
;;         ("v" helm-toggle-visible-mark)
;;         ("u" helm-toggle-all-marks)
;;         ("p" helm-execute-persistent-action)
;;         )

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
        :ensure t
        :config
        (add-hook 'after-init-hook #'global-flycheck-mode)
        (define-key evil-normal-state-map (kbd "SPC f") 'flycheck-error-list-explain-error)
        ))

    ;; (defhydra helm-like-unite (:hint nil
    ;;                            :pre (set-cursor-color "#e52b50")
    ;;                            :post (set-cursor-color "#ffffff")
    ;;                            ;; :color amaranth
    ;;                            )
    ;;   ("?" helm-help)
    ;;   ("<escape>" keyboard-escape-quit)
    ;;   ("<SPC>" helm-toggle-visible-mark "mark")
    ;;   ("v" helm-toggle-visible-mark "mark")
    ;;   ("a" helm-toggle-all-marks "(un)mark all")
    ;;   ;; not sure if there's a better way to do this
    ;;   ("/" (lambda ()
    ;;           (interactive)
    ;;           (execute-kbd-macro [?\C-s]))
    ;;        "search")
    ;;   ;; ("v" helm-execute-persistent-action)
    ;;   ;; suggested by Sylvain Benner (syl20bnr)
    ;;   ;; if you want to use a key besides TAB to go to action select and then exit the hydra
    ;;   ("r" helm-select-action :color blue)
    ;;   )

    ;; ;; (define-key helm-map (kbd "<escape>") 'helm-like-unite/body)

   (defun helm-my-buffers ()
    (interactive)
    (let ((helm-ff-transformer-show-only-basename nil))
    (helm-other-buffer '(helm-source-buffers-list
                        ;; helm-source-elscreen
                        helm-source-projectile-files-list
                        ;; helm-source-ctags
                        ;; helm-source-recentf
                        helm-source-locate)
                        "*helm-my-buffers*")))

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

    (define-key helm-map (kbd "<escape>") 'keyboard-escape-quit)
    (define-key helm-map (kbd "C-q") 'keyboard-escape-quit)

    (define-key helm-map (kbd "C-j") 'helm-next-line)
    (define-key helm-map (kbd "C-k") 'helm-previous-line)
    (define-key helm-map (kbd "C-h") 'helm-next-source)
    (define-key helm-map (kbd "C-l") 'helm-previous-source)
    (define-key helm-map (kbd "M-j") 'helm-next-line)
    (define-key helm-map (kbd "M-k") 'helm-previous-line)
    (define-key helm-map (kbd "M-h") 'helm-next-source)
    (define-key helm-map (kbd "M-l") 'helm-previous-source)

    (global-set-key (kbd "M-w") 'helm-buffers-list)
    (global-set-key (kbd "M-e") 'helm-my-buffers)
    (global-set-key (kbd "M-d") 'helm-for-files)
    (global-set-key (kbd "M-s") 'helm-buffers-list)

    (define-key evil-normal-state-map (kbd "SPC u ?") 'helm-apropos)
    (define-key evil-normal-state-map (kbd "SPC m m") 'helm-mini)
    (define-key evil-normal-state-map (kbd "SPC m f") 'helm-find)
    (define-key evil-normal-state-map (kbd "SPC m i") 'helm-semantic-or-imenu)
    (define-key evil-normal-state-map (kbd "DEL") 'helm-do-ag-this-file)

    (use-package helm-ag
      :ensure t
      :demand
      :config
      (progn
        )
     )

    (evil-leader/set-key
       ;; "q" 'find-file
       ;; "w" 'helm-for-files
       ;; "l" 'helm-locate
       ;; "y" 'helm-show-kill-ring
       ;; "t" 'helm-top
       ;; "m" 'helm-man-woman
       ;; "o" 'helm-occur
       ;; "j" 'helm-M-x
       ;; "e" 'helm-find-files
       ;; "b" 'helm-buffers-list
       ;; "f" 'helm-projectile-find-file
       ;; "r" 'helm-recentf
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
