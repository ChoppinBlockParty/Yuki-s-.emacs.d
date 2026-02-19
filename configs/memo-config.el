;;; memo-config --- Read-only memo pages for keybinding reference.
;;; Commentary:
;;; Code:

;; rg-mode keybinding entries for the memo
(defvar my-memo--rg-keybindings
  '("gcd  rg-rerun-change-dir       Re-run search in a different directory"
    "gf   rg-rerun-change-files     Re-run search with different file type"
    "gr   rg-recompile              Re-run the current search"
    "gm   rg-menu                   Open the rg transient menu"
    "gl   rg-list-searches          List saved searches"
    "zc   rg-rerun-toggle-case      Toggle case sensitivity"
    "zi   rg-rerun-toggle-ignore    Toggle ignore file handling"
    "zl   rg-rerun-change-literal   Switch to literal search"
    "zp   rg-rerun-change-regexp    Switch to regexp search"
    "zs   rg-save-search            Save the current search"
    "zS   rg-save-search-as-name    Save search with a name")
  "List of rg-mode keybinding entries.")

;; Display keybindings memo in the minibuffer via completing-read
(defun my-memo-keybindings ()
  "Show keybinding memo in the minibuffer."
  (interactive)
  (completing-read "Keybindings: " my-memo--rg-keybindings nil t))

(provide 'memo-config)
;;; memo-config.el ends here
