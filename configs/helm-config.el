;;; helm-config --- Helm power
;;; Commentary:
;;; Code:

(use-package helm
  :config
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
  ;; (helm-mode t)

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
    helm-comint-input-ring-fuzzy-match                 t ; everything will be fuzzy
    helm-mode-fuzzy-match                 t ; everything will be fuzzy
    helm-completion-in-region-fuzzy-match t
    helm-comint-input-ring-fuzzy-match    t
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
    (ido-buffer-internal 'display 'display-buffer nil nil nil 'ignore))

  (setq helm-display-function 'helm-default-display-buffer)
  (setq helm-adaptive-history-file "~/.emacs.d/.helm-adapative-history")

  (substitute-key-definition 'find-tag 'helm-etags-select global-map)
  (setq projectile-completion-system 'helm)

  (setq helm-command-prefix-key "C-c h")
  (setq helm-quick-update t)

  (after 'flycheck
    (use-package helm-flycheck
      :ensure t
      :config
      (add-hook 'after-init-hook #'global-flycheck-mode)
      (define-key evil-normal-state-map (kbd "SPC f") 'flycheck-error-list-explain-error)
      )
    )

  (defun helm-my-buffers ()
   (interactive)
   (let ((helm-ff-transformer-show-only-basename nil))
   (helm-other-buffer '(;; helm-source-elscreen
                        helm-source-projectile-files-list
                        helm-source-buffers-list
                       ;; helm-source-ctags
                       ;; helm-source-recentf
                       helm-source-locate)
                      "*helm-my-buffers*"))
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
                          "*helm jump*"))
     )

   (define-key helm-map (kbd "<escape>") 'keyboard-escape-quit)
   (define-key helm-map (kbd "C-q") 'keyboard-escape-quit)

   (define-key helm-map (kbd "C-k") 'helm-previous-line)
   (define-key helm-map (kbd "C-j") 'helm-next-line)
   (define-key helm-map (kbd "C-h") 'helm-previous-source)
   (define-key helm-map (kbd "C-l") 'helm-next-source)
   (define-key helm-map (kbd "M-k") 'helm-previous-line)
   (define-key helm-map (kbd "M-j") 'helm-next-line)
   (define-key helm-map (kbd "M-h") 'helm-previous-source)
   (define-key helm-map (kbd "M-l") 'helm-next-source)
)

(provide 'helm-config)
;;; helm-config.el ends here
