(use-package company
  :ensure t
  :init
  (progn
    (global-company-mode)
    )
  :config
  (progn

    (setq company-idle-delay 0.1
          company-minimum-prefix-length 1
          company-auto-complete nil
          company-require-match nil
          company-tooltip-limit 20                       ; bigger popup window
          company-echo-delay 0                           ; remove annoying blinking
          company-begin-commands '(self-insert-command)  ; start autocompletion only after typing
          company-dabbrev-ignore-case nil
          company-dabbrev-downcase nil
          company-frontends '(company-pseudo-tooltip-frontend)
          company-clang-prefix-guesser 'company-mode/more-than-prefix-guesser)

    (defvar-local company-fci-mode-on-p nil)

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
    ; (define-key company-active-map "\e\e\e" 'company-abort)
    (define-key company-active-map "\C-i" 'company-abort)
    ;; (define-key company-active-map [tab] 'company-complete-selection)
    ;; (define-key company-active-map (kbd "TAB") 'company-complete-selection)
    ; (define-key company-active-map [shift-tab] 'company-select-previous)
    (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
    (define-key company-active-map (kbd "TAB") 'company-complete-selection)
    ; (define-key company-active-map [tab] 'company-select-next)
    (define-key company-active-map (kbd "<f1>") 'company-show-doc-buffer)
    (define-key company-active-map "\C-w" 'company-show-location)
    (define-key company-active-map "\C-s" 'company-search-candidates)
    ; (define-key company-active-map "\C-\M-s" 'company-filter-candidates)
    (define-key company-active-map (kbd "RET") 'nil)
    (define-key company-active-map (kbd "<return>") 'nil)
    (define-key company-active-map (kbd "SPC") nil)
    (define-key company-active-map (kbd "C-SPC") 'company-complete-selection)
    ; (define-key company-active-map [return] 'company-abort)
)

  (custom-set-faces
       '(company-tooltip-common
         ((t (:inherit company-tooltip :weight bold :underline nil))))
       '(company-tooltip-common-selection
         ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
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
        (set-variable 'ycmd-server-command '("python" "/home/qwert/Vim/Plugins/Ycm/Plugin/third_party/ycmd"))
        (company-ycmd-setup)
        (use-package flycheck-ycmd
          :ensure t
          :init (add-hook 'ycmd-mode-hook 'flycheck-ycmd-setup))
      )
    )
  )
)

(provide 'config-autocompletion)
