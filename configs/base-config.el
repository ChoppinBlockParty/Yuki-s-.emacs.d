;;; base-config --- Basic Emacs configuration
;;; Commentary:
;;; Code:

;;; Default is fundamental-mode, but it act weirdly sometimes: on `new`
;;; command it does not enable all minor modes (e.g. does enable
;;; undo-tree-mode).
;; Seems like there is no fundamental-mode-hook.
;; use (setq-default minor-mode t)
(setq-default major-mode 'text-mode)
(setq initial-major-mode 'text-mode)

;;; Disable confirmations to kill processes.
(setq confirm-kill-processes nil)
;;; Disable warnings.
(setq native-comp-async-report-warnings-errors nil)
;;; Disable toolbar (must use -1 to disable)
(tool-bar-mode -1)
;;; Line numbers!
;;; Disable vertical scrollbars in all frames.
(scroll-bar-mode -1)
(defun my/disable-scroll-bars (frame)
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions 'my/disable-scroll-bars)
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
; (global-linum-mode t)

;;; require a trailing newline
;;; otherwise people get angry on you
(setq require-final-newline t)

(blink-cursor-mode 0)
;; (setq-default cursor-type 'bar)
;;; Emacs cursor, there are also evil cursor configurations
;; (set-cursor-color "#ff6c6b")

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

;;; Disable symlink the confirmation on symlink redirection.
(setq vc-follow-symlinks t)

;;; I prefer cmd key for meta
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

(if (string= system-type "darwin")
  (progn
    (setq default-frame-alist '((width . 127) (height . 47)))
    (select-frame-set-input-focus (selected-frame))
    (set-frame-font "DejaVuSansM Nerd Font Mono-13")
  )
  (progn
    (set-frame-font "DejaVu Sans Mono-13")
  )
  )


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

;;; Set random seed
;;; With argument t, set the random number seed from the system’s entropy pool if available,
;;; otherwise from less-random volatile data such as the time
(random t)

(setq
  backup-directory-alist         '(("." . "~/.cache/emacs/backup/"))
  auto-save-file-name-transforms '((".*" "~/.cache/emacs/autosave/" t))
  ;;; When Emacs exits normally, it deletes this file; if Emacs crashes, you can look in
  ;;; the file to find all the auto-save files that might contain work that was otherwise
  ;;; lost.
  auto-save-list-file-prefix     "~/.cache/emacs/autosave/.saves-"
  )

;;; Change emacs' title
(setq-default frame-title-format '("Emacs %f [%m]"))


;;; Defaults for big files
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
      ;; Best description of what electric means - https://emacs.stackexchange.com/a/3016/20171.
      (electric-indent-mode 1)
      ;; In past for some unknown reason I had disabled electric mode.
      ;(electric-indent-mode -1)
      ;; to override the electric-indent-mode state set by any major mode
      ;(add-hook 'after-change-major-mode-hook (lambda() (electric-indent-mode -1)))
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


;;; Turn off startup message in minibuffer
(defun display-startup-echo-area-message ()
  "Display nothing."
  (message nil))


;;; Close minibuffer when focus is lost due to mouse changing focus
(defun stop-using-minibuffer ()
  "Kill the minibuffer."
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))
(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)


;;; Disable messages flooding minibuffer
(defun my-command-error-function (data context caller)
  "Ignore the buffer-read-only, beginning-of-buffer,
end-of-buffer signals; pass the rest to the default handler."
  (when (not (memq (car data) '(buffer-read-only
                                beginning-of-buffer
                                end-of-buffer)))
    (command-error-default-function data context caller)))

(setq command-error-function #'my-command-error-function)


;;; Configure help mode.
(defun my-setup-help-mode ()
  "Setup help mode the way I like it."
  (set-fill-column 80))
(add-hook 'help-mode-hook 'my-setup-help-mode)


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
    recentf-max-saved-items 100000
    recentf-max-menu-items 5000
    )
  (recentf-mode 1)

  ;;; By default, Recentf saves the list of recent files on exiting
  ;;; Emacs (specifically, `recentf-save-list` is called on
  ;;; `kill-emacs-hook`). If Emacs exits abruptly for some reason the
  ;;; recent file list will be lost.
  ;;; Does not work after system suspension:
  ;;; Warning (emacs): recentf mode: Doing chmod: No such file or directory, /home/yuki/.cache/emacs/recentf
  ; (run-at-time nil (* 5 60) 'recentf-save-list)
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
    'typescript-mode-hook
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
;;; Don't remember why I reset I map, causes troubles for `dired-jump`
;;; in fundamental-mode buffers
;;; Real dired configuration is in `dired-config`.
(use-package dired
  :ensure nil
  :init
  ;; Need to be removed
  (defvar dired-mode-map (let ((map (make-keymap))) (set-keymap-parent map special-mode-map) map))
  )

(use-package flyspell
  :init
  (defvar flyspell-mode-map (let ((map (make-sparse-keymap))) map))
  )

;;; Required by ycmd.el
(use-package f)

(dolist (hook my-prog-modes-hook-list)
  ;; (add-hook hook 'linum-mode)
  )

(provide 'base-config)
;;; base-config.el ends here
