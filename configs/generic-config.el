;;; generic-config --- General settings
;;; Commentary:
;;; Code:

(use-package message
  :ensure nil
  :config
  (modify-syntax-entry ?_ "w" message-mode-syntax-table)
  (modify-syntax-entry ?- "w" message-mode-syntax-table)
  )

(use-package log-edit
  :ensure nil
  :config
  (modify-syntax-entry ?_ "w" log-edit-mode-syntax-table)
  (modify-syntax-entry ?- "w" log-edit-mode-syntax-table)
  )

(use-package url-util
  :ensure nil
  :config
  (modify-syntax-entry ?_ "w" url-parse-args-syntax-table)
  (modify-syntax-entry ?- "w" url-parse-args-syntax-table)
  )

(use-package url-cookie
  :ensure nil
  :config
  (modify-syntax-entry ?_ "w" url-cookie-mode-syntax-table)
  (modify-syntax-entry ?- "w" url-cookie-mode-syntax-table)
  )

(setq special-mode-map
  (let ((map (make-sparse-keymap)))
    (evil-define-key 'normal map
      "q"  'quit-window
      "gr" 'revert-buffer
      "gg" 'beginning-of-buffer
      "G"  'end-of-buffer
      )
    map))

(use-package compile
  :init
  (defvar compilation-minor-mode-map
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map special-mode-map)
      (define-key map [mouse-2] 'compile-goto-error)
      (define-key map [follow-link] 'mouse-face)
      (evil-define-key 'normal map
        (kbd "<return>") 'compile-goto-error
        (kbd "<S-return>") 'compilation-display-error
        "\C-c\C-c" 'kill-compilation
        "<" 'compilation-previous-error
        ">" 'compilation-next-error
        "}" 'compilation-previous-file
        "{" 'compilation-next-file
        "gr" 'recompile ; revert
        )
      map)
    )
  )

(use-package grep
  :init
  (defvar grep-mode-map
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map compilation-minor-mode-map)
      (evil-define-key 'normal map
        "zf"     'next-error-follow-minor-mode
        "zww"    'read-only-mode
        "\r"     'compile-goto-error
        "<"      'previous-error-no-select
        ">"      'next-error-no-select
        "}"      'compilation-previous-file
        "{"      'compilation-next-file
        )
      map)
    )
  :config
  (modify-syntax-entry ?_ "w" grep-mode-syntax-table)
  )

;;; Just need to reset `dired-mode-map` as early as possible,
;;; here feels like a fine place to do that.
;;; Real dired configuration is in `dired-config`.
(use-package dired
  :ensure nil
  :init
  (defvar dired-mode-map
    (let ((map (make-keymap)))
      (set-keymap-parent map special-mode-map)
      map)
    "Local keymap for Dired mode buffers.")
  )

(use-package with-editor
  :config
  (evil-define-key 'normal with-editor-mode-map
    "q" 'with-editor-finish
    "ZQ" 'with-editor-cancel
    "ZZ" 'with-editor-finish
    )
  )

(use-package doc-view
  :config
  (setq
    doc-view-continuous t
    )

  (evil-set-initial-state 'doc-view-mode 'normal)

  (evil-define-key 'normal doc-view-mode-map
    "q" 'quit-window
    (kbd "C-j") 'doc-view-next-page
    (kbd "C-k") 'doc-view-previous-page
    "gj" 'doc-view-next-page
    "gk" 'doc-view-previous-page
    (kbd "C-d") 'forward-page
    "j" 'doc-view-next-line-or-next-page
    "k" 'doc-view-previous-line-or-previous-page
    "gg" 'doc-view-first-page
    "G" 'doc-view-last-page
    "J" 'doc-view-goto-page
    (kbd "<return>") 'image-next-line

    ;; zoom
    "0" 'doc-view-scale-reset
    (kbd "C--") 'doc-view-shrink
    (kbd "C-=") 'doc-view-enlarge

    "W" 'doc-view-fit-width-to-window ; Like evil-image.
    "H" 'doc-view-fit-height-to-window ; Like evil-image.
    "P" 'doc-view-fit-page-to-window
    "X" 'doc-view-kill-proc

    (kbd "ss") 'doc-view-set-slice
    (kbd "sm") 'doc-view-set-slice-using-mouse
    (kbd "sb") 'doc-view-set-slice-from-bounding-box
    (kbd "sr") 'doc-view-reset-slice

    (kbd "/") 'doc-view-search
    (kbd "?") 'doc-view-search-backward
    (kbd "C-t") 'doc-view-show-tooltip
    (kbd "C-c C-c") 'doc-view-toggle-display
    (kbd "C-c C-t") 'doc-view-open-text

    ;; refresh
    (kbd "gr") 'doc-view-revert-buffer)

  (when evil-want-C-u-scroll
    (evil-define-key 'normal doc-view-mode-map
      (kbd "C-u") 'backward-page))
  )

(use-package image-mode
  :ensure nil
  :config
  (use-package image+)
  ;; TODO: pdf and doc-view conflict with image.
  ;; See https://github.com/emacs-evil/evil-collection/issues/23.

  (evil-set-initial-state 'image-mode 'normal)

  (evil-define-key 'normal image-mode-map
    ;; motion
    "gg" 'image-bob
    "G" 'image-eob
    "h" 'image-backward-hscroll
    "l" 'image-forward-hscroll
    "j" 'image-next-line
    "k" 'image-previous-line
    "0" 'image-bol
    "^" 'image-bol
    "$" 'image-eol
    (kbd "C--") 'imagex-sticky-zoom-out
    (kbd "C-=") 'imagex-sticky-zoom-in
    "M" 'imagex-sticky-maximize
    "O" 'imagex-sticky-restore-original
    "S" 'imagex-sticky-save-image
    "r" 'imagex-sticky-rotate-left
    "R" 'imagex-sticky-rotate-right
    (kbd "C-u") 'image-scroll-down
    (kbd "C-i") 'image-scroll-up
    (kbd "M-u") 'image-scroll-down
    (kbd "M-i") 'image-scroll-up
    ;; animation
    (kbd "<return>") 'image-toggle-animation
    "a0" 'image-reset-speed
    "ar" 'image-reverse-speed
    "F" 'image-goto-frame
    "," 'image-previous-frame ; mplayer/mpv style
    "." 'image-next-frame ; mplayer/mpv style
    ";" 'image-next-frame ; Evil style
    "{" 'image-decrease-speed ; mplayer/mpv style
    "}" 'image-increase-speed ; mplayer/mpv style

    "H" 'image-transform-fit-to-height
    "W" 'image-transform-fit-to-width

    "{" 'image-previous-file
    "}" 'image-next-file
    "gk" 'image-previous-file
    "gj" 'image-next-file
    (kbd "C-k") 'image-previous-file
    (kbd "C-j") 'image-next-file

    (kbd "C-c C-c") 'image-toggle-display

    ;; quit
    "q" 'quit-window
    "ZQ" 'evil-quit
    "ZZ" 'quit-window)
  )

(provide 'generic-config)
;;; generic-config.el ends here
