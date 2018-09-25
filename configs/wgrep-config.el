;;; wgre-config --- Writable grep-mode
;;; Commentary:
;;; Code:
(use-package wgrep
  :init
  (defvar wgrep-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map "\C-c\C-c" 'wgrep-finish-edit)
      (define-key map "\C-c\C-d" 'wgrep-mark-deletion)
      (define-key map "\C-c\C-e" 'wgrep-finish-edit)
      (define-key map "\C-c\C-p" 'wgrep-toggle-readonly-area)
      (define-key map "\C-c\C-r" 'wgrep-remove-change)
      (define-key map "\C-x\C-s" 'wgrep-finish-edit)
      (define-key map "\C-c\C-u" 'wgrep-remove-all-change)
      (define-key map "\C-c\C-[" 'wgrep-remove-all-change)
      (define-key map "\C-c\C-k" 'wgrep-abort-changes)
      (define-key map "\C-x\C-q" 'wgrep-exit)
      map))
  :config
  (setq wgrep-enable-key "zwg")

  (evil-define-key nil wgrep-mode-map [remap evil-write] 'wgrep-finish-edit)

  (evil-define-key 'normal wgrep-mode-map
    "ZQ" 'wgrep-abort-changes
    "ZZ" 'wgrep-finish-edit
    (kbd "<escape>") 'wgrep-exit)
  )
(provide 'wgrep-config)
;;; wgrep-config.el ends here
