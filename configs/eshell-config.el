;;; eshell-config --- Summary

;;; Commentary:
;;; Eshell configuration

;;; Code:
(after 'helm
(use-package eshell
  :ensure t
  :config
  (progn

    ; Remember lots of previous commands in shell-mode
    (setq comint-input-ring-size 1000000)
    (add-hook 'shell-mode-hook 'my-shell-mode-hook)
    (defun my-shell-mode-hook ()
      (setq comint-input-ring-file-name "~/.config/zsh/.zhistory")
                                        ; Ignore timestamps in history file.  Assumes that zsh
                                        ; EXTENDED_HISTORY option is in use.
      (setq comint-input-ring-separator "\n: \\([0-9]+\\):\\([0-9]+\\);")
      (comint-read-input-ring t))

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
      ;; (eshell-cmpl-initialize)
      ;; (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
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
        "[" 'eshell-previous-input
        "]" 'eshell-next-input
        (kbd "C-k") 'eshell-previous-input
        (kbd "C-j") 'eshell-next-input
        (kbd "M-k") 'eshell-previous-input
        (kbd "M-j") 'eshell-next-input
        "gk" 'eshell-previous-input
        "gj" 'eshell-next-input
        "0" 'eshell-bol
        "^" 'eshell-bol
        (kbd "M-h") 'eshell-backward-argument
        (kbd "M-l") 'eshell-forward-argument
        (kbd "<return>") 'eshell-send-input
        (kbd "C-c C-c") 'evil-collection-eshell-interrupt-process)
      (evil-define-key 'insert 'eshell-mode-map
        (kbd "C-r") 'helm-eshell-history
        ;; motion
        (kbd "C-k") 'eshell-previous-input
        (kbd "C-j") 'eshell-next-input
        (kbd "M-k") 'eshell-previous-input
        (kbd "M-j") 'eshell-next-input
        (kbd "C-h") 'eshell-backward-argument
        (kbd "C-l") 'eshell-forward-argument)
      (evil-define-key 'visual 'shell-mode-map
        ;; motion
        ;; TODO: This does not work with `evil-visual-line'.
        (kbd "C-k") 'eshell-previous-input
        (kbd "C-j") 'eshell-next-input
        (kbd "M-k") 'eshell-previous-input
        (kbd "M-j") 'eshell-next-input
        "gk" 'eshell-previous-input
        "gj" 'eshell-next-input
        "0" 'eshell-bol
        "^" 'eshell-bol)
      )

    (add-hook 'eshell-mode-hook 'my-evil-eshell-setup)
    (add-hook 'eshell-first-time-mode-hook 'evil-collection-eshell-setup-keys)

    )
  )

(require 'comint)
(setq comint-prompt-read-only t
      comint-input-ignoredups t)
;; Part of emacs
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only)
  )
(defun my-comint-send-input-maybe ()
  "Only `comint-send-input' when point is after the latest prompt.
Otherwise move to the end of the buffer."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (if (and proc (>= (point) (marker-position (process-mark proc))))
        (comint-send-input)
      (comint-copy-old-input)
      ;; (goto-char (point-max))
      )
    )
  )

(with-eval-after-load "comint"
  (define-key shell-mode-map [remap comint-send-input] 'my-comint-send-input-maybe)
  )

(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
;; For ansi-term
;; (ansi-color-for-comint-mode-on)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
(add-hook 'shell-mode-hook
      (lambda ()
        (face-remap-set-base 'comint-highlight-prompt :inherit nil)))

;; You can highlight some text based on regexp (useful to see "OK" or warnings):
;; (add-hook 'shell-mode-hook (lambda () (highlight-regexp "\\[OK\\]" "hi-green-b")))
;; Make URLs clickable
(add-hook 'shell-mode-hook (lambda () (goto-address-mode )))
;; Make file paths clickable
;; Every line representing a path to a file will be colorized and made clickable, so that you can jump to that file and that line, like in compilation-mode (specially useful when compiling a program or running tests):
(add-hook 'shell-mode-hook 'compilation-shell-minor-mode)

(evil-define-key 'normal 'comint-mode-map
  (kbd "C-p") #'comint-previous-prompt
  (kbd "C-n") #'comint-next-prompt
  (kbd "M-p") #'comint-previous-prompt
  (kbd "M-n") #'comint-next-prompt
  (kbd "C-k") #'comint-previous-input
  (kbd "C-j") #'comint-next-input
  (kbd "M-k") #'comint-previous-input
  (kbd "M-j") #'comint-next-input
  (kbd "gk") #'comint-previous-input
  (kbd "gj") #'comint-next-input
  (kbd "<up>") #'comint-next-input
  (kbd "<down>") #'comint-previous-input)

(evil-define-key 'insert 'comint-mode-map
  (kbd "C-p") #'comint-previous-prompt
  (kbd "C-n") #'comint-next-prompt
  (kbd "M-p") #'comint-previous-prompt
  (kbd "M-n") #'comint-next-prompt
  (kbd "C-k") #'comint-previous-input
  (kbd "C-j") #'comint-next-input
  (kbd "M-k") #'comint-previous-input
  (kbd "M-j") #'comint-next-input
  (kbd "<up>") #'comint-previous-input
  (kbd "<down>") #'comint-next-input)

)

(provide 'eshell-config)
;;; eshell-config.el ends here
