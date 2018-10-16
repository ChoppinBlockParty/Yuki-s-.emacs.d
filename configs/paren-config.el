;;; paren-config --- Rainbow parentheses
;;; Commentary:
;;; Code:

;;; show-paren-mode allows one to see matching pairs of parentheses and other characters.
;;; When point is on the opening character of one of the paired characters, the other is
;;; highlighted. When the point is after the closing character of one of the paired
;;; characters, the other is highlighted.
(use-package paren
  :config
  (setq
    show-paren-delay 0
    ;;; Do not do this
    ;; show-paren-style 'expression
    show-paren-style 'parenthesis
    )
  (show-paren-mode t)
)

;;; "rainbow parentheses"-like mode which highlights delimiters such as parentheses,
;;; brackets or braces according to their depth.
;;; https://github.com/Fanael/rainbow-delimiters
(use-package rainbow-delimiters
  :config
  (dolist (hook (append my-prog-modes-hook-list my-markup-modes-hook-list))
    (add-hook hook #'rainbow-delimiters-mode))

  (require 'color)
  (cl-loop
    for index from 1 to rainbow-delimiters-max-face-count
    do
    (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
      (cl-callf color-saturate-name (face-foreground face) 75)
      ))

  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :foreground 'unspecified
                      :inherit 'error
                      :strike-through t)

  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :foreground 'unspecified
                      :inherit 'show-paren-mismatch)

  )

(provide 'paren-config)
;;; paren-config.el ends here
