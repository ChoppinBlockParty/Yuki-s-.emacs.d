;;; eshell-config --- Summary

;;; Commentary:
;;; Eshell configuration

;;; Code:
(after 'helm
(use-package eshell
  :ensure t
  :config
  (progn

    (defconst evil-collection-eshell-maps '(eshell-mode-map))

    (defun evil-collection-eshell-next-prompt ()
    "`evil' wrapper around `eshell-next-prompt'."
    (when (get-text-property (point) 'read-only)
        ;; If at end of prompt, `eshell-next-prompt' will not move, so go backward.
        (beginning-of-line)
        (eshell-next-prompt 1)))

    (defun evil-collection-eshell-next-prompt-on-insert ()
      "Go to next prompt on `evil' replace/insert enter."
      (dolist (hook '(evil-replace-state-entry-hook evil-insert-state-entry-hook))
        (add-hook hook 'evil-collection-eshell-next-prompt nil t)
        )
      )

    (defun my-evil-eshell-setup ()
      ""
      (eshell-cmpl-initialize)
      (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
      (evil-collection-eshell-next-prompt-on-insert)
      )

    (defun evil-collection-eshell-interrupt-process ()
      "Interupt `eshell' process and enter insert state."
      (interactive)
      (eshell-interrupt-process)
      (evil-insert t)
      )

    ;;; `eshell-mode-map' is reset when Eshell is initialized in `eshell-mode'. We
    ;;; need to add bindings to `eshell-first-time-mode-hook'.
    (defun evil-collection-eshell-setup-keys ()
      "Set up `evil' bindings for `eshell'."
      ;; (define-key eshell-mode-map (kbd "C-r") 'helm-eshell-history)
      (evil-define-key 'normal 'eshell-mode-map
        (kbd "C-r") 'helm-eshell-history
        ;; motion
        "[" 'eshell-previous-prompt
        "]" 'eshell-next-prompt
        (kbd "C-k") 'eshell-previous-prompt
        (kbd "C-j") 'eshell-next-prompt
        (kbd "M-k") 'eshell-previous-prompt
        (kbd "M-j") 'eshell-next-prompt
        "gk" 'eshell-previous-prompt
        "gj" 'eshell-next-prompt
        "0" 'eshell-bol
        "^" 'eshell-bol
        (kbd "M-h") 'eshell-backward-argument
        (kbd "M-l") 'eshell-forward-argument
        (kbd "<return>") 'eshell-send-input
        (kbd "C-c C-c") 'evil-collection-eshell-interrupt-process)
      (evil-define-key 'insert 'eshell-mode-map
        (kbd "C-r") 'helm-eshell-history
        ;; motion
        (kbd "C-k") 'eshell-previous-prompt
        (kbd "C-j") 'eshell-next-prompt
        (kbd "C-h") 'eshell-backward-argument
        (kbd "C-l") 'eshell-forward-argument)
      (evil-define-key 'visual 'shell-mode-map
        ;; motion
        ;; TODO: This does not work with `evil-visual-line'.
        "[" 'eshell-previous-prompt
        "]" 'eshell-next-prompt
        (kbd "C-k") 'eshell-previous-prompt
        (kbd "C-j") 'eshell-next-prompt
        "gk" 'eshell-previous-prompt
        "gj" 'eshell-next-prompt
        "0" 'eshell-bol
        "^" 'eshell-bol)
      )

    (add-hook 'eshell-mode-hook 'my-evil-eshell-setup)
    (add-hook 'eshell-first-time-mode-hook 'evil-collection-eshell-setup-keys)

    )
  )

(require 'comint)
(evil-define-key 'normal 'comint-mode-map
  (kbd "C-k") #'comint-previous-input
  (kbd "C-j") #'comint-next-input
  (kbd "gk") #'comint-previous-input
  (kbd "gj") #'comint-next-input
  (kbd "]") #'comint-next-input
  (kbd "[") #'comint-previous-input)

(evil-define-key 'insert 'comint-mode-map
  (kbd "C-k") #'comint-previous-input
  (kbd "C-j") #'comint-next-input
  (kbd "<up>") #'comint-previous-input
  (kbd "<down>") #'comint-next-input)

)

(provide 'eshell-config)
;;; eshell-config.el ends here
