;;; file-modes-config --- Miscellaneous file modes.
;;; Commentary:
;;; Code:
(use-package clang-format)

(defun my-formatting ()
  ""
  (interactive)
  (cond
    ((equal major-mode 'go-mode) (gofmt))
    ((equal major-mode 'c++-mode) (clang-format-buffer))
    (t nil))
  )

(define-key evil-normal-state-map (kbd "SPC s") 'my-formatting)

(require 'protobuf-mode)
(modify-syntax-entry ?_ "w" protobuf-mode-syntax-table)
(modify-syntax-entry ?- "w" protobuf-mode-syntax-table)
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))

(use-package cmake-mode
  :config
  (modify-syntax-entry ?_ "w" cmake-mode-syntax-table)
  (modify-syntax-entry ?- "w" cmake-mode-syntax-table)
  )

(use-package go-mode
  :config
  (modify-syntax-entry ?_ "w" go-mode-syntax-table)
  (modify-syntax-entry ?- "w" go-mode-syntax-table)
  )

(use-package markdown-mode
  :config
  (modify-syntax-entry ?_ "w" markdown-mode-syntax-table)
  (modify-syntax-entry ?- "w" markdown-mode-syntax-table)
  )

(use-package vimrc-mode
  :config
  (modify-syntax-entry ?_ "w" vimrc-mode-syntax-table)
  (modify-syntax-entry ?- "w" vimrc-mode-syntax-table)
  (add-to-list 'auto-mode-alist '("\\.vim\\'" . vimrc-mode))
  (add-to-list 'auto-mode-alist '("[._]?g?vimrc\\'" . vimrc-mode))
  (add-to-list 'auto-mode-alist '("\\.exrc\\'" . vimrc-mode))
  )

(use-package dockerfile-mode
  :config
  (modify-syntax-entry ?_ "w" dockerfile-mode-syntax-table)
  (modify-syntax-entry ?- "w" dockerfile-mode-syntax-table)
  )
(use-package docker-compose-mode
  :config
  (modify-syntax-entry ?_ "w" docker-compose-mode-syntax-table)
  (modify-syntax-entry ?- "w" docker-compose-mode-syntax-table)
  )

(provide 'file-modes-config)
;;; file-modes-config.el ends here
