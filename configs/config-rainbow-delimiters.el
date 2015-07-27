(after 'cl-lib
(after 'color
(use-package rainbow-delimiters
  :ensure t
  :config
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'latex-mode-hook 'rainbow-delimiters-mode)

    (cl-loop
     for index from 1 to rainbow-delimiters-max-face-count
     do
     (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
       (cl-callf color-saturate-name (face-foreground face) 30)))
      )

    (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                        :foreground 'unspecified
                        :inherit 'error
                        :strike-through t)

    (require 'paren) ; show-paren-mismatch is defined in paren.el
    (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                        :foreground 'unspecified
                        :inherit 'show-paren-mismatch)

   ;(defvar my-paren-dual-colors
   ;  '("hot pink" "dodger blue"))

   ;(setq rainbow-delimiters-outermost-only-face-count 0)
   ;(setq rainbow-delimiters-max-face-count 2)

   ;(set-face-foreground 'rainbow-delimiters-depth-1-face
   ;                     (elt my-paren-dual-colors 1))
   ;(set-face-foreground 'rainbow-delimiters-depth-2-face
   ;                      (Elt my-paren-dual-colors 0))

    ; (defface my-outermost-paren-face
    ;   '((t (:weight bold)))
    ;   "Face used for outermost parens.")

    ; (setq rainbow-delimiters-outermost-only-face-count 1)
    ; (set-face-attribute 'rainbow-delimiters-depth-1-face nil
    ;                     :foreground 'unspecified
    ;                     :inherit 'my-outermost-paren-face)

)))

(provide 'config-rainbow-delimiters)
