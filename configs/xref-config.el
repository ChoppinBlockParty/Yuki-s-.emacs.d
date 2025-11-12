;;; xref-config --- xref mode configuration
;;; Commentary: Happens when, for example, there are multiple choices in a code jump.
;;; Code:

(after 'evil-config
    (require 'my-evil-collection-xref)
    (evil-collection-xref-setup)
  )

(provide 'xref-config)
;;; xref-config.el ends here
