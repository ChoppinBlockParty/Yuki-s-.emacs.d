;;; memo-config --- Read-only memo pages for keybinding reference.
;;; Commentary:
;;; Code:

;; Display a read-only memo buffer with rg-mode keybindings
(defun my-memo-keybindings ()
  "Show a read-only memo of rg-mode keybindings."
  (interactive)
  (my-memo--show "*memo: rg keybindings*"
   "rg-mode Keybindings (Evil Normal State)\n"
   "========================================\n\n"
   "Navigation\n"
   "----------\n"
   "  gcd  rg-rerun-change-dir       Re-run search in a different directory\n"
   "  gf   rg-rerun-change-files     Re-run search with different file type\n"
   "  gr   rg-recompile              Re-run the current search\n"
   "  gm   rg-menu                   Open the rg transient menu\n"
   "  gl   rg-list-searches          List saved searches\n\n"
   "Toggles\n"
   "-------\n"
   "  zc   rg-rerun-toggle-case      Toggle case sensitivity\n"
   "  zi   rg-rerun-toggle-ignore    Toggle ignore file handling\n"
   "  zl   rg-rerun-change-literal   Switch to literal search\n"
   "  zp   rg-rerun-change-regexp    Switch to regexp search\n\n"
   "Saving\n"
   "------\n"
   "  zs   rg-save-search            Save the current search\n"
   "  zS   rg-save-search-as-name    Save search with a name\n"))

;; Helper to display a read-only memo buffer with given content
(defun my-memo--show (name &rest content)
  "Create a read-only memo buffer called NAME with CONTENT strings.
Uses `special-mode' so q closes the buffer."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (apply #'concat content))
        (insert "\n[press q to close]"))
      (goto-char (point-min))
      (special-mode))
    (pop-to-buffer buf)))

(provide 'memo-config)
;;; memo-config.el ends here
