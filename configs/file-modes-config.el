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
  (modify-syntax-entry ?_ "w" c-mode-syntax-table)
  (modify-syntax-entry ?_ "w" c-identifier-syntax-table)
  (modify-syntax-entry ?_ "w" c-no-parens-syntax-table)
  (modify-syntax-entry ?_ "w" java-mode-syntax-table)
  (modify-syntax-entry ?- "w" java-mode-syntax-table)
  (modify-syntax-entry ?_ "w" objc-mode-syntax-table)
  (modify-syntax-entry ?- "w" objc-mode-syntax-table)
  )

(use-package sh-script
  :config
  (modify-syntax-entry ?_ "w" sh-mode-syntax-table)
  (modify-syntax-entry ?- "w" sh-mode-syntax-table)
  )

(use-package diff-mode
  :config
  (modify-syntax-entry ?_ "w" diff-mode-syntax-table)
  (modify-syntax-entry ?- "w" diff-mode-syntax-table)
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

  (modify-syntax-entry ?_ "w" godoc-mode-syntax-table)
  (modify-syntax-entry ?- "w" godoc-mode-syntax-table)

  (evil-define-key 'normal go-mode-map
    (kbd "SPC d") 'godef-jump
    (kbd "SPC c") 'pop-tag-mark)
  (evil-add-command-properties #'godef-jump :jump t)
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
  :ensure t
  ;; :load-path "local/lua-mode"
  :config
  (modify-syntax-entry ?_ "w" lua-mode-syntax-table)
  )

(use-package lispy
  :config
  (add-hook 'emacs-lisp-mode-hook (lambda ()
                                    (lispy-mode 1)))
  )

;; For javascript-eslint function
(use-package js2-mode)

(use-package web-mode
  :config
  (modify-syntax-entry ?_ "w" web-mode-syntax-table)
  ;;; auto-enable for .js/.jsx files
  (add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
  (setq
    ;;; Enable JSX syntax highlighting in .js/.jsx files
    web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))
    web-mode-enable-auto-quoting nil
   )

  (use-package prettier-js)

  (use-package add-node-modules-path)
  (add-hook 'flycheck-mode-hook 'add-node-modules-path)

  ;;; Enable eslint checker for web-mode
  (flycheck-add-mode 'javascript-eslint 'web-mode)

  (defun web-mode-init-hook ()
    "Hooks for Web mode.  Adjust indent."
    (setq web-mode-markup-indent-offset 2
          web-mode-markup-indent-offset 2
          web-mode-css-indent-offset 2
          web-mode-code-indent-offset 2
          web-mode-script-padding 2
          web-mode-attr-indent-offset 2
          web-mode-enable-css-colorization t)
    (add-node-modules-path)
    (prettier-js-mode)
    )

  (add-hook 'web-mode-hook 'web-mode-init-hook)

  (use-package tide
    :config
    (defun setup-tide-mode()
      ;; (interactive)
      (tide-setup)
      (setq flycheck-check-syntax-automatically '(save mode-enabled))
      (tide-hl-identifier-mode 1)
      )

    ;; aligns annotation to the right hand side
    (setq company-tooltip-align-annotations t)

    (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)

    ;; formats the buffer before saving
    ; (add-hook 'before-save-hook 'tide-format-before-save)

    (add-hook 'typescript-mode-hook #'setup-tide-mode)
    (add-hook 'web-mode-hook #'setup-tide-mode)
    )
  )


(provide 'file-modes-config)
;;; file-modes-config.el ends here
