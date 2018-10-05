;;; file-modes-config --- Miscellaneous file modes.
;;; Commentary:
;;; Code:
(use-package cc-mode
  :ensure nil
  :config
  (dolist (hook (list 'c++-mode-hook 'c-mode-hook))
    (add-hook hook (lambda() (add-to-list 'ispell-skip-region-alist '("^#include" forward-line)))))
  ;;; To highlight functions
  (dolist (mode (list 'c++-mode 'c-mode))
    (font-lock-add-keywords mode '(("\\<\\(\\sw+\\) ?(" 1 'font-lock-function-name-face))))
  (modify-syntax-entry ?_ "w" c++-mode-syntax-table)
  (modify-syntax-entry ?_ "w" c++-template-syntax-table)
  (modify-syntax-entry ?_ "w" c-mode-syntax-table)
  (modify-syntax-entry ?_ "w" c-identifier-syntax-table)
  (modify-syntax-entry ?_ "w" c-no-parens-syntax-table)
  )

(use-package protobuf-mode
  :ensure nil
  :load-path "local/protobuf"
  :config
  (modify-syntax-entry ?_ "w" protobuf-mode-syntax-table)
  (modify-syntax-entry ?- "w" protobuf-mode-syntax-table)
  (add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))
  )

(use-package cmake-mode
  :config
  (modify-syntax-entry ?_ "w" cmake-mode-syntax-table)
  (modify-syntax-entry ?- "w" cmake-mode-syntax-table)
  )

(use-package go-mode
  :config
  (modify-syntax-entry ?_ "w" go-mode-syntax-table)
  )

(use-package markdown-mode
  :config
  (modify-syntax-entry ?_ "w" markdown-mode-syntax-table)
  (modify-syntax-entry ?- "w" markdown-mode-syntax-table)
  )

(use-package vimrc-mode
  :config
  (modify-syntax-entry ?_ "w" vimrc-mode-syntax-table)
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

(use-package lua-mode
  :ensure nil
  :load-path "local/lua-mode"
  :config
  (modify-syntax-entry ?_ "w" lua-mode-syntax-table)
  )


(provide 'file-modes-config)
;;; file-modes-config.el ends here
