;;; vterm-config.el --- my VTerm configuration for Emacs
;;; Commentary:
;;; Code:
;;;


(use-package vterm
  :after (evil)
  :ensure t
  :init
    (setq vterm-always-compile-module t)
    (setq vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=no")
  :config
    (setq vterm-max-scrollback 1000000)
    (require 'my-evil-collection-vterm)
    (evil-collection-vterm-setup)
    (evil-define-key 'insert vterm-mode-map
      (kbd "C-c") #'vterm--self-insert
      )
  )

(provide 'vterm-config)
;;; vterm-config.el ends here
