;;; base-config --- Basic Emacs configuration
;;; Commentary:
;;; Code:

;;; Disable toolbar (must use -1 to disable)
(tool-bar-mode -1)
;;; Line numbers!
;;; Disable vertical scrollbars in all frames.
(scroll-bar-mode -1)
;;; Disable menu bar for all (trying this out)
(menu-bar-mode -1)
;;; No dialog boxes, rather show message in minibuffer
(setq use-dialog-box nil)

;;; Break long lines at word boundaries
(visual-line-mode t)

;;; Removes duplicated items everywhere
(setq-default history-delete-duplicates t)

;;; Lock files are evil...
(setq create-lockfiles nil)

;;; also tabs are evil...
(setq-default indent-tabs-mode nil)
;;; With this setting the TAB key will first try to re-indent the current line. If the line is already indented properly it will call completion-at-point instead.
(setq-default tab-always-indent 'complete)

;;; number columns in the status bar
(column-number-mode)

;;; show line numbers
;; (global-linum-mode t)

;;; require a trailing newline
;;; otherwise people get angry on you
(setq require-final-newline t)

(blink-cursor-mode 0)
(setq-default cursor-type 'bar)
;;; Emacs cursor, there are also evil cursor configurations
(set-cursor-color "#ff6c6b")

;;; Seems to be nice but has nasty bugs
;; (use-package smooth-scrolling
;;   :config
;;   (smooth-scrolling-mode 1)
;; )

;;; The variable redisplay-dont-pause, when set to t, will cause Emacs to
;;; fully redraw the display before it processes queued input events.
;;; This may have slight performance implications if you’re aggressively
;;; mouse scrolling a document or rely on your keyboard’s auto repeat
;;; feature. For most of us, myself included, it’s probably a no-brainer
;;; to switch it on.
(setq
  redisplay-dont-pause t
  ;; Always redraw immediately when scrolling,
  ;; more responsive and doesn't hang!
  fast-but-imprecise-scrolling nil
  jit-lock-defer-time 0
  scroll-margin 2
  scroll-step 1
  scroll-preserve-screen-position t)
(setq scroll-conservatively 1) ;; move minimum when cursor exits view, instead of recentering
(setq mouse-wheel-scroll-amount '(1)) ;; mouse scroll moves 1 line at a time, instead of 5 lines
(setq mouse-wheel-progressive-speed nil) ;; on a long mouse scroll keep scrolling by 1 line


;;; Enable the mouse in terminal mode.
(xterm-mouse-mode 1)

;;; Change screen position for some file types (e.g. javascript)
;;; probably due to formatter on save
; (setq auto-save-timeout 2)
; (auto-save-visited-mode)


;;; On graphical displays, each Emacs window normally has narrow fringes (gutters/margins) on the left and right edges. The fringes are used to display symbols that provide information about the text in the window. You can type M-x fringe-mode to disable the fringes, or modify their width. This command affects fringes in all frames; to modify fringes on the selected frame only, use M-x set-fringe-style. You can make your changes to the fringes permanent by customizing the variable fringe-mode.
;; Out-of-the-box the most common use of the fringes is to indicate a continuation line. When one line of text is split into multiple screen lines, the left fringe shows a curving arrow for each screen line except the first, indicating that “this is not the real beginning”. The right fringe shows a curving arrow for each screen line except the last, indicating that “this is not the real end”. If the line’s direction is right-to-left, the meanings of the curving arrows in the fringes are swapped.
;; Third-party modes like flycheck and diff-hl also make use of the fringe to display valuable information there (e.g. lint and VC information).
(set-fringe-style '(8 . 0))

;; don't put intitial text in scratch buffer
(setq initial-scratch-message nil)

;;; https://stackoverflow.com/questions/18316665/how-to-improve-emacs-performance-when-view-large-file
;;; Should improve large file performance
(defun my-find-file-check-make-large-file-read-only-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) (* 10 1024 1024))
    (setq buffer-read-only t)
    (linum-mode -1)
    (font-lock-mode -1)
    (buffer-disable-undo)
    (fundamental-mode)))
(add-hook 'find-file-hook 'my-find-file-check-make-large-file-read-only-hook)

;;; Amazing hook, makes life more easier when opening a new file.
;;; Whenever a file with a non-existent directory is visited, emacs offers to create the
;;; parent directories.
(defun my-create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
      (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
    (make-directory parent-directory t))))
(add-to-list 'find-file-not-found-functions #'my-create-non-existent-directory)

;'Woman' > 'man'.
(defalias 'man 'woman)

;; from <https://github.com/bling/dotemacs/>
(defmacro after (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,feature
     '(progn ,@body)))

;;; Hide startup messages
(setq inhibit-splash-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-message t)

;;; Ediff with horizontal splits.
(setq ediff-split-window-function 'split-window-horizontally)

;;; Let me write `y` or `n` even for important stuff that would normally require
;;; me to fully type `yes` or `no`.
(defalias 'yes-or-no-p 'y-or-n-p)

;;; UTF-8 everything!
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;; This isn't a typewriter (even if it is a terminal); one space after sentences,
;;; please.
(setq sentence-end-double-space nil)

;;; Flash the frame to represent a bell.
(setq visible-bell t)
;;; nevermind that's annoying
(setq ring-bell-function 'ignore)

;;; The default of 16 is too low. Give me a 64-object mark ring.
;;; Across all files, make it 128.
(setq mark-ring-max 64)
(setq global-mark-ring-max 128)

;;; Display the current function name in the modeline.
;;; Annoying..
(which-function-mode -1)

;;; Show me the new saved file if the contents change on disk when editing.
(global-auto-revert-mode 1)

(defvar my-last-preferred-splits '())

(defun my-reuse-last-preferred-split-save (win new-win)
  "Save WIN NEW-WIN for reuse."
  (when new-win
    (add-to-list 'my-last-preferred-splits `(,win . ,new-win))
    )
  new-win)

(defun my-reuse-last-preferred-split (window)
  "Reuse split for WINDOW."
  (let ((new-list '()))
    (dolist (val my-last-preferred-splits)
      (when (and (window-live-p (car val)) (window-live-p (cdr val)))
        (add-to-list 'new-list val)
        )
      )
    (setq my-last-preferred-splits new-list)
  )
  (let ((old-win (cdr (assq window my-last-preferred-splits))))
    old-win)
  )

(setq split-window-preferred-function (lambda (&optional window)
  "Split WINDOW."
  (let
    ((window (or window (selected-window))))
    (or
      (my-reuse-last-preferred-split window)
      (and (window-splittable-p window)
           (my-reuse-last-preferred-split-save
             window
             (with-selected-window window (split-window-below))
             )
           )
      (and (window-splittable-p window t)
           (my-reuse-last-preferred-split-save
             window
             (with-selected-window window (split-window-right))
             )
           )
      (and
        ;; If WINDOW is the only usable window on its frame (it is
        ;; the only one or, not being the only one, all the other
        ;; ones are dedicated) and is not the minibuffer window, try
        ;; to split it vertically disregarding the value of
        ;; `split-height-threshold'.
        (let ((frame (window-frame window)))
          (or
           (eq window (frame-root-window frame))
           (catch 'done
             (walk-window-tree (lambda (w)
                                 (unless (or (eq w window)
                                             (window-dedicated-p w))
                                   (throw 'done nil)))
                               frame)
             t)))
         (not (window-minibuffer-p window))
         (let ((split-height-threshold 0))
              (when (window-splittable-p window)
                    (my-reuse-last-preferred-split-save
                      window
                      (with-selected-window window (split-window-below))
                      )
                    )
              )
         )
      (and
         (let ((split-height-threshold 0))
              (when (window-splittable-p window)
                    (my-reuse-last-preferred-split-save
                      window
                      (with-selected-window window (split-window-below))
                      )
                    )
              )
         )
    ))
  ))

(defun my-display-buffer-find-major-mode-window (mode &optional mode-two)
  "Find window. MODE to look up. MODE-TWO optional."
  (let (cur-win ret-win (l (window-list)))
    (while (and l (not ret-win))
           (setq cur-win (car l)
                 l       (cdr l))
           (with-current-buffer (window-buffer cur-win)
             (cond ((equal major-mode mode)     (setq ret-win cur-win))
                   ((equal major-mode mode-two) (setq ret-win cur-win))
                   ))
           )
    ret-win))

(defun my-window-display-buffer-create-split (window)
  "Create split relative to WINDOW."
  (let
    (new-win)
    (setq new-win (window--try-to-split-window window))
    (unless new-win
      (setq new-win (window--try-to-split-window (get-lru-window t t))))
    (unless new-win
      (setq new-win window))
    (unless new-win
      (setq new-win (selected-window)))
    new-win))

(defun my-window-display-buffer (buffer window)
  "Display. BUFFER to display. WINDOW to show into."
  (if (or (window-minibuffer-p) (window-dedicated-p))
      (window--display-buffer
        buffer
        (my-window-display-buffer-create-split window)
        'reuse '((inhibit-switch-frame . t)))
      (window--display-buffer
        buffer
        window
        'reuse '((inhibit-switch-frame . t)))
      ))

(defun my-window-display-buffer-split (buffer)
  "Split. BUFFER to display relative to WIN or `selected-window`."
  (my-window-display-buffer buffer
                            (my-window-display-buffer-create-split (selected-window))
                            ))

(defun my-window-display-buffer-match-any (str &rest matchers)
  "Match STR to any of MATCHERS."
  (let (match-p m res)
    (while (and (not match-p) matchers)
      (setq m (car matchers)
            matchers (cdr matchers)
            res (string-match-p m str)
            )
      (when (and res (= res 0)) (setq match-p t))
      )
    match-p)
  )

(defun my-display-buffer-action (buffer alist)
  "Display BUFFER in the selected window with ALIST."
  (let
    ((sel-buf (window-buffer (selected-window))) sel-mode new-mode)
    (with-current-buffer sel-buf
      (setq sel-mode major-mode))
    (with-current-buffer buffer
      (setq new-mode major-mode))
    ;; (print sel-mode)
    ;; (print (buffer-name sel-buf))
    ;; (print new-mode)
    ;; (print (buffer-name buffer))
    ;; (print alist)
    (cond
      ((or (equal new-mode 'apropos-mode) (equal new-mode 'help-mode))
       (let ((win (my-display-buffer-find-major-mode-window 'apropos-mode 'help-mode)))
         (if win
             (my-window-display-buffer buffer win)
             (my-window-display-buffer-split buffer)
             ))
       )
      ;;; For magit pop-ups that are called transient with a space in front
      ((and
         (equal new-mode 'fundamental-mode)
         (my-window-display-buffer-match-any (buffer-name buffer) "\\`\s-*\\*transient\\*\\'")
         )
       (my-window-display-buffer
         buffer
         (with-selected-window (selected-window) (split-window-below)))
       )
      ((or
         (member new-mode '(dired-sidebar-mode
                            magit-popup-mode
                            rg-mode
                            ivy-occur-grep-mode
                            completion-list-mode))
         (my-window-display-buffer-match-any (buffer-name buffer) "\\`\\*Completions\\*\\'")
         )
       nil
       )
      ((or
         (and (equal sel-mode 'text-mode)
              (my-window-display-buffer-match-any (buffer-name sel-buf) "\\`COMMIT_EDITMSG"))
         (equal sel-mode 'magit-log-mode)
         )
       (my-window-display-buffer-split buffer)
       )
      ((equal sel-mode 'dired-sidebar-mode)
       (my-window-display-buffer buffer (get-mru-window nil nil t))
       )
      (t
       (if (cdr (assq 'inhibit-same-window alist))
           (my-window-display-buffer-split buffer)
           (my-window-display-buffer buffer (selected-window))
           )
       )
      )
    ))
(add-to-list 'display-buffer-alist '(".*" (my-display-buffer-action)))

(defun my-setup-file-defaults ()
  "Check the size of files when loading, and don't let me break them."
  (if (> (buffer-size) (* 10 1024 1024))
      (progn
        (setq buffer-read-only t)
        (buffer-disable-undo)
        (fundamental-mode)
        t
        )
      (setq show-trailing-whitespace t)
      (electric-indent-mode 1)
      ))
(add-hook 'find-file-hook 'my-setup-file-defaults)

(defun display-startup-echo-area-message ()
  (message nil))

(defun my-do-not-kill-scratch-buffers ()
  "Don't let the scratch buffer die."
  (if (member (buffer-name (current-buffer)) '("*scratch*" "*Messages*"))
      (progn
        (bury-buffer)
        nil)
    t))
(add-hook 'kill-buffer-query-functions 'my-do-not-kill-scratch-buffers)

(random t) ;; seed

(defun my-setup-help-mode ()
  "Setup help mode the way I like it."
  (set-fill-column 80))
(add-hook 'help-mode-hook 'my-setup-help-mode)

;; (set-frame-font "Inconsolata-dz for Powerline-11")
(set-frame-font "DejaVu Sans Mono-11")
;; (set-frame-font "Hack-10")

(setq
  backup-directory-alist         '(("." . "~/.cache/emacs/backup/"))
  auto-save-file-name-transforms '((".*" "~/.cache/emacs/autosave/" t))
  ;;; When Emacs exits normally, it deletes this file; if Emacs crashes, you can look in
  ;;; the file to find all the auto-save files that might contain work that was otherwise
  ;;; lost.
  auto-save-list-file-prefix     "~/.cache/emacs/autosave/.saves-"
  )

(defun my-command-error-function (data context caller)
  "Ignore the buffer-read-only, beginning-of-buffer,
end-of-buffer signals; pass the rest to the default handler."
  (when (not (memq (car data) '(buffer-read-only
                                beginning-of-buffer
                                end-of-buffer)))
    (command-error-default-function data context caller)))

(setq command-error-function #'my-command-error-function)

;;; Remember my latest place when opening a new file.
(use-package saveplace
  :config
  (setq save-place-file "~/.cache/emacs/saveplace")
  (save-place-mode 1)
  )
(use-package savehist
  :config
  (setq
    savehist-file "~/.cache/emacs/savehist"
    savehist-additional-variables '(search ring regexp-search-ring)
    savehist-autosave-interval 60
    )
  (savehist-mode t)
  )
(use-package recentf
  :config
  (setq
    recentf-save-file "~/.cache/emacs/recentf"
    recentf-max-saved-items 1000
    recentf-max-menu-items 500
    )
  (recentf-mode 1)
  )

(defconst my-prog-modes-hook-list
  (list
    'c++-mode-hook
    'c-mode-hook
    'c-mode-common-hook
    'cmake-mode-hook
    'makefile-mode-hook
    'sh-mode-hook
    'dockerfile-mode-hook
    'docker-compose-mode-hook
    'go-mode-hook
    'python-mode-hook
    'java-mode-hook
    'objc-mode-hook
    'lisp-mode-hook
    'emacs-lisp-mode-hook
    'latex-mode-hook
    'vimrc-mode-hook
    'lua-mode-hook
    'crontab-mode-hook
    'js2-mode-hook
    'web-mode-hook
    'scheme-mode-hook
    'clojure-mode-hook
    'ruby-mode-hook
    'yaml-mode-hook
    'css-mode-hook
    'php-mode-hook
    'perl-mode-hook
    'haskell-mode-hook
    'org-mode-hook
    'caml-mode-hook
    'rst-mode-hook
    'tcl-mode-hook
    )
  )

(defconst my-markup-modes-hook-list
  (list
    'fundamental-mode-hook
    'git-commit-mode-hook
    'markdown-mode-hook
    'latex-mode-hook
    'org-mode-hook
    'text-mode-hook
    )
  )

(use-package clang-format)

(defun my-buffer-formatting ()
  "Format buffer."
  (interactive)
  (cond
    ((equal major-mode 'go-mode)         (gofmt))
    ((equal major-mode 'c++-mode)        (clang-format-buffer))
    ((equal major-mode 'c-mode)          (clang-format-buffer))
    ((equal major-mode 'protobuf-mode)   (clang-format-buffer))
    (t nil))
  )

;;; Just need to reset the maps as early as possible, here feels like
;;; a fine place to do that.
;;; Real dired configuration is in `dired-config`.
(use-package dired
  :ensure nil
  :init
  (defvar dired-mode-map (let ((map (make-keymap))) (set-keymap-parent map special-mode-map) map))
  )
(use-package flyspell
  :init
  (defvar flyspell-mode-map (let ((map (make-sparse-keymap))) map))
  )

(provide 'base-config)
;;; base-config.el ends here
