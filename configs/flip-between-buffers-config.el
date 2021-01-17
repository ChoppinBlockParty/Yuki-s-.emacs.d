;;; init --- Emacs startup script
;;; Commentary:
;;; Code:

(use-package iflipb
  :after (evil)
  :config
  (define-key evil-motion-state-map (kbd "3") 'iflipb-next-buffer)
  (define-key evil-motion-state-map (kbd "4") 'iflipb-previous-buffer)
  )

(provide 'flip-between-buffers-config)
;;; flip-between-buffers-config.el ends here
