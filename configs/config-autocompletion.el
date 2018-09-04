(use-package company
   :ensure t
   :init
   (progn
     (add-hook 'after-init-hook 'global-company-mode)
     )
   :config
   (progn

     (setq company-idle-delay 0.0
           company-minimum-prefix-length 1
           ;; company-auto-complete nil
           ;; company-require-match nil                      ; cancel selections by typing non-matching character
           ;; company-tooltip-limit 20                       ; bigger popup window
           company-echo-delay 0                           ; remove annoying blinking
           ;; company-begin-commands '(self-insert-command)  ; start autocompletion only after typing
           ;; company-dabbrev-ignore-case nil
           ;; company-dabbrev-downcase nil
           ;; company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
           ;;                     company-preview-frontend
           ;;                     company-echo-metadata-frontend)
           ;; company-clang-prefix-guesser 'company-mode/more-than-prefix-guesser
           )
  ;; Described in [Switch from AC](https://github.com/company-mode/company-mode/wiki/Switching-from-AC)
  ;; (defun my-company-visible-and-explicit-action-p ()
  ;;   (and (company-tooltip-visible-p)
  ;;        (company-explicit-action-p)))
  (defun company-preview-if-not-tng-frontend (command)
      "`company-preview-frontend', but not when tng is active."
      (unless (and (eq command 'post-command)
                  company-selection-changed
                  (memq 'company-tng-frontend company-frontends))
      (company-preview-frontend command)))
  (defun company-ac-setup ()
    "Sets up `company-mode' to behave similarly to `auto-complete-mode'."
    (setq company-require-match nil)
    (setq company-auto-complete #'my-company-visible-and-explicit-action-p)
    (setq company-auto-complete t)
    (setq company-frontends '(company-tng-frontend
                              company-echo-metadata-frontend
                              company-pseudo-tooltip-unless-just-one-frontend
                              company-preview-if-not-tng-frontend))
    ;; (define-key company-active-map [tab]
    ;;   'company-select-next-if-tooltip-visible-or-complete-selection)
    ;; (define-key company-active-map (kbd "TAB")
    ;;   'company-select-next-if-tooltip-visible-or-complete-selection)
    )

    ;; Big discussion on how run Tab completion
    ;;https://github.com/company-mode/company-mode/pull/706
    (company-ac-setup)

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
     (evil-define-key 'insert company-mode-map (kbd "ESC") 'company-abort)
     ; (define-key company-active-map "\e\e\e" 'company-abort)
     (define-key company-active-map "\C-i" 'company-abort)
     (define-key company-active-map (kbd "[shift-tab]") 'company-select-previous)
     (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
     (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
     ;; (define-key company-active-map (kbd "TAB") 'company-complete-selection)
     ;; (global-set-key (kbd "TAB") #'company-indent-or-complete-common)
     (define-key company-active-map (kbd "[tab]") 'company-complete-common-or-cycle)
     (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
     (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
     ; (define-key company-active-map [tab] 'company-select-next)
     (define-key company-active-map (kbd "<f1>") 'company-show-doc-buffer)
     (define-key company-active-map (kbd "C-w") 'company-abort)
     (define-key company-active-map "\C-s" 'company-search-candidates)
     ; (define-key company-active-map "\C-\M-s" 'company-filter-candidates)
     ;; (add-hook 'shell-mode-hook
     ;;           (make-local-variable 'company-auto-complete)
     ;;           (setq company-auto-complete nil)
     ;;           (lambda()
     ;;             (define-key company-active-map (kbd "RET") (lambda()
     ;;                                                          (interactive)
     ;;                                                               (company-abort)
     ;;                                                               (comint-send-input)
     ;;                                                               )
     ;;             )
     ;;           )
     ;;           )
     ;; https://github.com/company-mode/company-mode/pull/706#issuecomment-348309127
     (defun company-quit ()
       "Insert any selected completion and quit completing."
       (interactive)
       (when (and company-selection-changed company--manual-action
	          (boundp 'company-tng--overlay) company-tng--overlay)
         (company--insert-candidate
          (nth company-selection company-candidates)))
       (company-cancel))
     (define-key company-active-map [return] 'company-quit)
     (define-key company-active-map (kbd "RET") 'company-quit)

     (define-key company-active-map (kbd "SPC") nil)
     (evil-define-key 'insert 'global (kbd "C-SPC") 'company-complete-common)

    (use-package ycmd
    :ensure t
    :config
    (progn
        (set-variable 'ycmd-server-command `("python3" ,(file-truename "~/Data/Sources/ycmd/ycmd")))
        (set-variable 'ycmd-global-config "~/.ycm_extra_conf.py")
        ;; (add-hook 'after-init-hook #'global-ycmd-mode)
        (use-package company-ycmd
        :ensure t
        :init (company-ycmd-setup)
        :config
        (progn
            ;; (add-to-list 'company-backends (company-mode/backend-with-yas 'company-ycmd))
            (use-package flycheck-ycmd
                :ensure t
                :init (add-hook 'ycmd-mode-hook 'flycheck-ycmd-setup))
        )
        )
        ;; (add-hook 'after-init-hook #'global-ycmd-mode)
        (add-hook 'python-mode-hook 'ycmd-mode)
        (add-hook 'c-mode-hook 'ycmd-mode)
        (add-hook 'c++-mode-hook 'ycmd-mode)
        (add-hook 'go-mode-hook 'ycmd-mode)
    )
    )
)

;   (custom-set-faces
;        '(company-tooltip-common
;          ((t (:inherit company-tooltip :weight bold :underline nil))))
;        '(company-tooltip-common-selection
;          ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
)

(provide 'config-autocompletion)
