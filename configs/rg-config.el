;;; rg-config --- RG
;;; Commentary:
;;; Code:
(use-package rg
  :init
  (defvar rg-mode-map
    (let ((map (copy-keymap grep-mode-map)))
      (evil-define-key 'normal map
        "gcd"  'rg-rerun-change-dir
        "gf"   'rg-rerun-change-files
        "gl"   'rg-list-searches
        "gr"   'rg-recompile
        "zc"   'rg-rerun-toggle-case
        "zi"   'rg-rerun-toggle-ignore
        "zl"   'rg-rerun-change-literal
        "zp"   'rg-rerun-change-regexp
        "zs"   'rg-save-search
        "zS"   'rg-save-search-as-name
        )
      map)
  "The global keymap for `rg'.")
  :config
  (setq
    rg-keymap-prefix nil
    rg-ignore-case 'smart
    )
  (add-to-list 'rg-custom-type-aliases '("cpp" . "*.h"))
  (evil-set-initial-state 'rg-mode 'normal)

  (modify-syntax-entry ?_ "w" rg-mode-syntax-table)
  )

(provide 'rg-config)
;;; rg-config.el ends here
