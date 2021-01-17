;;; init --- Emacs startup script
;;; Commentary:
;;; Code:

(use-package iflipb
  :after (evil dired magit-config rg-config)
  :config

  (setq
    iflipb-ignore-buffers '("^[*]" "^magit: " "^magit-process: ")
    iflipb-wrap-around 't
    )

  (define-key evil-motion-state-map (kbd "3") 'iflipb-next-buffer)
  (define-key evil-motion-state-map (kbd "4") 'iflipb-previous-buffer)
  (evil-define-key 'normal dired-mode-map
    (kbd "3") 'iflipb-next-buffer
    (kbd "4") 'iflipb-previous-buffer
    )

  (evil-magit-define-key 'normal 'magit-mode-map "3" 'iflipb-next-buffer)
  (evil-magit-define-key 'normal 'magit-mode-map "4" 'iflipb-previous-buffer)
  (evil-magit-define-key 'normal 'magit-diff-mode-map "3" 'iflipb-next-buffer)
  (evil-magit-define-key 'normal 'magit-diff-mode-map "4" 'iflipb-previous-buffer)

  (evil-magit-define-key 'normal 'rg-mode-map "3" 'iflipb-next-buffer)
  (evil-magit-define-key 'normal 'rg-mode-map "4" 'iflipb-previous-buffer)
  )

(provide 'flip-between-buffers-config)
;;; flip-between-buffers-config.el ends here
