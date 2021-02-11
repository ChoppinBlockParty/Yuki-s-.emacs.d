;;; other-stuff-config --- Other stuff
;;; Commentary:
;;; Code:

;;; https://www.emacswiki.org/emacs/UndoTree
;;; Otherwise redo does not work
(use-package undo-tree
  :config
  (global-undo-tree-mode)
  )

;;; https://emacs.stackexchange.com/questions/9583/how-to-treat-underscore-as-part-of-the-word
(modify-syntax-entry ?_ "w" (standard-syntax-table))
(modify-syntax-entry ?- "w" (standard-syntax-table))
(modify-syntax-entry ?_ "w" emacs-lisp-mode-syntax-table)
(modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table)
(modify-syntax-entry ?_ "w" xml-syntax-table)
(modify-syntax-entry ?- "w" xml-syntax-table)
(modify-syntax-entry ?_ "w" prog-mode-syntax-table)
(modify-syntax-entry ?- "w" prog-mode-syntax-table)
(modify-syntax-entry ?_ "w" url-parse-syntax-table)
(modify-syntax-entry ?- "w" url-parse-syntax-table)
(modify-syntax-entry ?_ "w" help-mode-syntax-table)
(modify-syntax-entry ?- "w" help-mode-syntax-table)
(modify-syntax-entry ?_ "w" text-mode-syntax-table)
(modify-syntax-entry ?- "w" text-mode-syntax-table)
(modify-syntax-entry ?_ "w" font-lock-syntax-table)
(modify-syntax-entry ?- "w" font-lock-syntax-table)
(modify-syntax-entry ?_ "w" lisp-mode-syntax-table)
(modify-syntax-entry ?- "w" lisp-mode-syntax-table)
(modify-syntax-entry ?_ "w" Info-mode-syntax-table)
(modify-syntax-entry ?- "w" Info-mode-syntax-table)
(modify-syntax-entry ?_ "w" occur-mode-syntax-table)
(modify-syntax-entry ?- "w" occur-mode-syntax-table)
;; (modify-syntax-entry ?_ "w" comint-mode-syntax-table)
;; (modify-syntax-entry ?- "w" comint-mode-syntax-table)
(modify-syntax-entry ?_ "w" special-mode-syntax-table)
(modify-syntax-entry ?- "w" special-mode-syntax-table)
(modify-syntax-entry ?_ "w" epa-key-mode-syntax-table)
(modify-syntax-entry ?- "w" epa-key-mode-syntax-table)
(modify-syntax-entry ?_ "w" epa-info-mode-syntax-table)
(modify-syntax-entry ?- "w" epa-info-mode-syntax-table)
(modify-syntax-entry ?_ "w" occur-edit-mode-syntax-table)
(modify-syntax-entry ?- "w" occur-edit-mode-syntax-table)
(modify-syntax-entry ?_ "w" change-log-mode-syntax-table)
(modify-syntax-entry ?- "w" change-log-mode-syntax-table)
(modify-syntax-entry ?_ "w" Buffer-menu-mode-syntax-table)
(modify-syntax-entry ?- "w" Buffer-menu-mode-syntax-table)
(modify-syntax-entry ?_ "w" process-menu-mode-syntax-table)
(modify-syntax-entry ?- "w" process-menu-mode-syntax-table)
(modify-syntax-entry ?_ "w" edit-abbrevs-mode-syntax-table)
(modify-syntax-entry ?- "w" edit-abbrevs-mode-syntax-table)
(modify-syntax-entry ?_ "w" package-menu-mode-syntax-table)
(modify-syntax-entry ?- "w" package-menu-mode-syntax-table)
(modify-syntax-entry ?_ "w" tabulated-list-mode-syntax-table)
(modify-syntax-entry ?- "w" tabulated-list-mode-syntax-table)
(modify-syntax-entry ?_ "w" completion-list-mode-syntax-table)
(modify-syntax-entry ?- "w" completion-list-mode-syntax-table)
(modify-syntax-entry ?_ "w" messages-buffer-mode-syntax-table)
(modify-syntax-entry ?- "w" messages-buffer-mode-syntax-table)
(modify-syntax-entry ?_ "w" lisp-interaction-mode-syntax-table)
(modify-syntax-entry ?- "w" lisp-interaction-mode-syntax-table)
(modify-syntax-entry ?_ "w" epa-key-list-mode-syntax-table)
(modify-syntax-entry ?- "w" epa-key-list-mode-syntax-table)
(modify-syntax-entry ?_ "w" evil-list-view-mode-syntax-table)
(modify-syntax-entry ?- "w" evil-list-view-mode-syntax-table)
(modify-syntax-entry ?_ "w" elisp-byte-code-mode-syntax-table)
(modify-syntax-entry ?- "w" elisp-byte-code-mode-syntax-table)
(modify-syntax-entry ?_ "w" evil-command-window-mode-syntax-table)
(modify-syntax-entry ?- "w" evil-command-window-mode-syntax-table)
(modify-syntax-entry ?_ "w" use-package-statistics-mode-syntax-table)
(modify-syntax-entry ?- "w" use-package-statistics-mode-syntax-table)

(provide 'other-stuff-config)
;;; other-stuff-config.el ends here
