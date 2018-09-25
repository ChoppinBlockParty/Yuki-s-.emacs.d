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

;;; tabs width
(setq-default tab-width 2)

;;; number columns in the status bar
(column-number-mode)

;;; show line numbers
;; (global-linum-mode t)

;;; require a trailing newline
;;; otherwise people get angry on you
(setq require-final-newline t)

;;; (must use -1 to disable)
(blink-cursor-mode -1)

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

;; (use-package color-theme-approximate
;;   :ensure t
;;   :demand
;;   :config
;;   (progn
;;     (color-theme-approximate-on)
;;   )
;; )

;;; Hide startup messages
(setq inhibit-splash-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-message t)

;;; Ediff with horizontal splits.
(setq ediff-split-window-function 'split-window-horizontally)

;;; Only scroll one line when near the bottom of the screen, instead
;;; of jumping the screen around.
(setq scroll-conservatively 9999
      scroll-preserve-screen-position t)

;;; Let me write `y` or `n` even for important stuff that would normally require
;;; me to fully type `yes` or `no`.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Enable the mouse in terminal mode.
(xterm-mouse-mode 1)

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

;;; Open help windows in current window, not other
(add-to-list 'display-buffer-alist '("*Help*" display-buffer-same-window))
(add-to-list 'display-buffer-alist '("*Apropos*" display-buffer-same-window))

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
    'git-commit-mode-hook
    'c++-mode-hook
    'c-mode-hook
    'cmake-mode-hook
    'shell-script-mode
    'makefile-mo
    'dockerfile-mode-hook
    'docker-compose-mode-hook
    'go-mode-hook
    'python-mode-hook
    'java-mode-hook
    'objc-mode-hook
    'lisp-mode-hook
    'emacs-lisp-mode-hook
    'markdown-mode-hook
    'latex-mode-hook
    'vimrc-mode-hook
    'lua-mode-hook
    'crontab-mode-hook
    'js2-mode-hook
    'scheme-mode-hook
    'clojure-mode-hook
    'ruby-mode-hook
    'yaml-mode
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
    ((equal major-mode 'go-mode)  (gofmt))
    ((equal major-mode 'c++-mode) (clang-format-buffer))
    ((equal major-mode 'c-mode)   (clang-format-buffer))
    (t nil))
  )

(provide 'base-config)
;;; base-config.el ends here
