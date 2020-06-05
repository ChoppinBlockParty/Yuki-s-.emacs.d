;;; color-theme-config --- Marvelous Moe theme
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

;; (use-package zerodark-theme
;;   :config
;;   (setq
;;    zerodark-use-paddings-in-mode-line nil
;;    )
;;   (load-theme 'zerodark t)
;;   (custom-theme-set-faces
;;    'zerodark
;;   ;;; To make all-the-icons work we need to disable some basic inheritance, because zerodark theme changes the height, that results in graphical glitchies with mode line
;;    '(powerline-active0            ((t (:foreground "#61259e" :background "#d0d0f0" :inherit mode-line))))
;;    '(powerline-inactive0          ((t (:background "black")))) ; :inherit mode-line))))
;;    '(powerline-active2            ((t (:foreground "#68f3ca" :background "black")))) ; :inherit mode-line))))
;;    '(powerline-inactive2          ((t (:background "grey20")))) ; :inherit mode-line-inactive))))
;;    '(font-lock-type-face          ((t (:foreground "#5fafaf"))))
;;    '(font-lock-variable-name-face ((t (:foreground "#d7875f"))))
;;    '(show-paren-match             ((t (:background "#7f1de1" :foreground "#1f1623" :weight extra-bold))))
;;    '(completions-annotations      ((t (:foreground "#ff6c6b"))))
;;    '(completions-common-part      ((t (:foreground "#4e5079"))))
;;    '(completions-first-difference ((t (:foreground "#da8548" :weight bold))))
;;    )
;;   )

;; (use-package kaolin-themes
;;   :config
;;   (load-theme 'kaolin-bubblegum t)
;;   (kaolin-treemacs-theme)
;;   )

;; Too green
;; (use-package spacemacs-dark-theme
;;   :ensure spacemacs-theme
;;   :config
;;   )

;; Nice, bright theme
(use-package moe-theme
  ;;; Important to be after powerline otherwise overrides faces
  :after (powerline-config)
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
  ;; (moe-light)
  (moe-dark)
  (custom-set-faces
   '(ivy-minibuffer-match-face-1 ((t (:background "dark orchid" :foreground "#eeeeee" :weight bold))))
   '(ivy-minibuffer-match-face-2 ((t (:background "dark orchid" :foreground "#eeeeee" :weight bold))))
   '(ivy-minibuffer-match-face-3 ((t (:background "dark orchid" :foreground "#eeeeee" :weight bold))))
   '(ivy-minibuffer-match-face-4 ((t (:background "dark orchid" :foreground "#eeeeee" :weight bold))))
  )
  (defface powerline-active1 '((t (:foreground "#d0d0f0" :background "purple" :inherit mode-line)))
    "Powerline face 1."
    :group 'powerline)
  (defface powerline-active2 '((t (:foreground "#63b132" :background "black" :inherit mode-line)))
    "Powerline face 2."
    :group 'powerline)
)


(provide 'color-theme-config)
;;; color-theme-config.el ends here
