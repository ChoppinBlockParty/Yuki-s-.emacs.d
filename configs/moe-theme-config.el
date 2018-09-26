;;; moe-theme-config --- Marvelous Moe theme
;;; Commentary:
;;; Code:

;;; Sharp dark theme
;; (use-package cyberpunk-theme
;;   :ensure t
;;   :config
;;   (load-theme 'cyberpunk t)
;;   )
;;; Super sharp dark theme
;; (use-package grandshell-theme
;;   :ensure t
;;   :config
;;   (load-theme 'grandshell t)
;;   )
(use-package moe-theme
 ;;; Important to be after powerline otherwise overrides faces
 :after(powerline-config)
 :config
 ;;; Show highlighted buffer-id as decoration. (Default: nil)
 (setq moe-theme-highlight-buffer-id t)

 ;;; Resize titles (optional).
 (setq moe-theme-resize-markdown-title '(1.5 1.4 1.3 1.2 1.0 1.0))
 (setq moe-theme-resize-org-title '(1.5 1.4 1.3 1.2 1.1 1.0 1.0 1.0 1.0))
 (setq moe-theme-resize-rst-title '(1.5 1.4 1.3 1.2 1.1 1.0))

 ;;; Choose a color for mode-line.(Default: blue)
 ;;; (Available colors: blue, orange, green ,magenta, yellow, purple, red, cyan, w/b.)
 (moe-theme-set-color 'purple)

 ;;; Finally, apply moe-theme now.
 ;;; Choose what you like, (moe-light) or (moe-dark)
 (moe-dark)
)

(provide 'moe-theme-config)
;;; moe-theme-config.el ends here
