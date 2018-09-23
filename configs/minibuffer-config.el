;;; minibuffer-config --- Configures minibuffer behavior
;;; Commentary:
;;; Code:
(dolist (map (list
               minibuffer-local-map
               minibuffer-local-ns-map
               minibuffer-local-completion-map
               minibuffer-local-must-match-map
               minibuffer-local-isearch-map))
  (define-key map (kbd "<escape>") 'abort-recursive-edit)
  )
(provide 'minibuffer-config)
;;; minibuffer-config.el ends here
