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
  :ensure nil
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
  :ensure nil
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

(provide 'generic-config)
;;; generic-config.el ends here
