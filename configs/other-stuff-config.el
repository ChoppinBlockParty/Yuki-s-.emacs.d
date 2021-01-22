;;; other-stuff-config --- Other stuff
;;; Commentary:
;;; Code:

;;; https://www.emacswiki.org/emacs/UndoTree
;;; Otherwise redo does not work
(use-package undo-tree
  :config
  (global-undo-tree-mode)
  )

(provide 'other-stuff-config)
;;; other-stuff-config.el ends here
