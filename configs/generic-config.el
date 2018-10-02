;;; generic-config --- General settings
;;; Commentary:
;;; Code:
(setq special-mode-map
  (let ((map (make-sparse-keymap)))
    (evil-define-key 'normal map
      "q" 'quit-window
      "gr" 'revert-buffer
      "gg" 'beginning-of-buffer
      "G" 'end-of-buffer
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
    (kbd "C-=") 'doc-view-enlarge
    (kbd "C--") 'doc-view-shrink

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

  ;; TODO: What if the user changes `evil-want-C-u-scroll' after this is run?
  (when evil-want-C-u-scroll
    (evil-define-key 'normal doc-view-mode-map
      (kbd "C-u") 'backward-page))
  )

(provide 'generic-config)
;;; generic-config.el ends here
