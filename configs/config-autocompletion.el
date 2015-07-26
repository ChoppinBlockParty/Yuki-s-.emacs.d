(use-package company
  :ensure company
  :init
  (progn
    (global-company-mode)
    )
  :config
  (progn

    (setq company-tooltip-limit 20)                      ; bigger popup window
    (setq company-idle-delay .1)                         ; decrease delay before autocompletion popup shows
    (setq company-echo-delay 0)                          ; remove annoying blinking
    (setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
    (setq company-minimum-prefix-length 1)

    (after 'evil
      ; (defun my-complete-or-indent ()
      ;     (interactive)
      ;     (if (company-manual-begin)
      ;         (company-complete-common)
      ;       (indent-according-to-mode))
      ; )
      (defun my-indent-or-complete ()
          (interactive)
          (if (looking-at "\\_>")
              (company-manual-begin)
            (indent-according-to-mode)))
      (define-key evil-insert-state-map (kbd "TAB") #'my-indent-or-complete)
    )

    (defun my-company-pass-key (arg)
      "Pass a key out of company-mode"
      (interactive "P")
      (company-abort)
      (kbd arg)
      )
    ;; C-hjkl in company-mode
    (define-key company-active-map (kbd "C-h") 'company-show-doc-buffer)
    (define-key company-active-map (kbd "C-l") 'company-show-location)
    (define-key company-active-map (kbd "C-j") 'company-select-next)
    (define-key company-active-map (kbd "C-k") 'company-select-previous)
    (define-key company-active-map (kbd "<down>") 'company-select-next-or-abort)
    (define-key company-active-map (kbd "<up>") 'company-select-previous-or-abort)
    (define-key company-active-map [down-mouse-1] 'ignore)
    (define-key company-active-map [down-mouse-3] 'ignore)
    (define-key company-active-map [mouse-1] 'ignore)
    (define-key company-active-map [mouse-3] 'ignore)
    (define-key company-active-map [up-mouse-1] 'ignore)
    (define-key company-active-map [up-mouse-3] 'ignore)
    ; (define-key company-active-map (kbd "ESC") 'company-abort)
    (evil-define-key 'insert company-mode-map (kbd "ESC") 'company-abort)
    (define-key company-active-map [return] 'company-complete-selection)
    (define-key company-active-map (kbd "SPC") 'my-company-pass-key)
    ; (define-key company-active-map "\e\e\e" 'company-abort)
    (define-key company-active-map "\C-i" 'company-abort)
    (define-key company-active-map [tab] 'company-select-next)
    ; (define-key company-active-map [shift-tab] 'company-select-previous)
    ; (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
    ; (define-key company-active-map (kbd "TAB") 'company-select-next)
    ; (define-key company-active-map [tab] 'company-select-next)
    (define-key company-active-map (kbd "<f1>") 'company-show-doc-buffer)
    (define-key company-active-map "\C-w" 'company-show-location)
    (define-key company-active-map "\C-s" 'company-search-candidates)
    ; (define-key company-active-map "\C-\M-s" 'company-filter-candidates)
    )

  (custom-set-faces
   '(company-preview
     ((t (:foreground "darkgray" :underline t))))
   '(company-preview-common
     ((t (:inherit company-preview))))
   '(company-tooltip
     ((t (:background "lightgray" :foreground "black"))))
   '(company-tooltip-selection
     ((t (:background "steelblue" :foreground "white"))))
   '(company-tooltip-common
     ((((type x)) (:inherit company-tooltip :weight bold))
      (t (:inherit company-tooltip))))
   '(company-tooltip-common-selection
     ((((type x)) (:inherit company-tooltip-selection :weight bold))
      (t (:inherit company-tooltip-selection)))))
)

(use-package ycmd
  :ensure t
  :config
  (progn
    (add-hook 'after-init-hook #'global-ycmd-mode)
    (use-package company-ycmd
      :ensure t
      :config
      (progn
        (company-ycmd-setup)
      )
    )
  )
)

(provide 'config-autocompletion)
