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
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))

(use-package cmake-mode)

(use-package go-mode)

(use-package markdown-mode)

(use-package vimrc-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.vim\\'" . vimrc-mode))
  (add-to-list 'auto-mode-alist '("[._]?g?vimrc\\'" . vimrc-mode))
  (add-to-list 'auto-mode-alist '("\\.exrc\\'" . vimrc-mode))
  )

(use-package dockerfile-mode)
(use-package docker-compose-mode)

(provide 'file-modes-config)
;;; file-modes-config.el ends here
