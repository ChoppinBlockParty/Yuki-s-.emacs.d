;;; shell-config --- Configures eshell and shell modes
;;; Commentary:
;;; Code:
(use-package eshell
  :after (counsel)
  :config
  ;; use helm to list eshell history
  ;; (add-hook 'eshell-mode-hook
  ;;         #'(lambda ()
  ;;             (substitute-key-definition 'eshell-list-history 'helm-eshell-history eshell-mode-map)))

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
      ;; (kbd "C-r") 'helm-eshell-history
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
      ;; (kbd "C-r") 'helm-eshell-history
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

(with-eval-after-load 'counsel
(with-eval-after-load 'helm
(defun my-shell ()
  "Similar to `shell`, but allows to spawn new shells on successive invocations."
  (interactive)
  (let ((currentbuf (get-buffer-window (current-buffer)))
        (newbuf     (generate-new-buffer-name "*shell*")))
    (generate-new-buffer newbuf)
    (set-window-dedicated-p currentbuf nil)
    (set-window-buffer currentbuf newbuf)
    (shell newbuf)
    )
  )

(require 'comint)
(use-package xterm-color :ensure t)

;;; Part of Emacs
;; (require 'ansi-color)

;;; `shell` uses (pop-to-buffer buffer) instead of (switch-to-buffer buffer)
;;; That fucks up windows when `shell` is called.
;;; https://github.com/syl20bnr/spacemacs/issues/6820#issuecomment-239665146
;; (push (cons "\\*shell\\*" display-buffer--same-window-action) display-buffer-alist)
;;; https://stackoverflow.com/questions/40301732/m-x-shell-open-shell-in-other-windows
(add-to-list 'display-buffer-alist `(,(regexp-quote "*shell") display-buffer-same-window))

(setq comint-prompt-read-only t
      ;;; Remember lots of previous commands in shell-mode
      comint-input-ring-size 100000
      comint-input-ignoredups t)
;;; For ANSI colors
;; (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

;;; For ANSI colors
;; (defun colorize-compilation-buffer ()
;;   (toggle-read-only)
;;   (ansi-color-apply-on-region (point-min) (point-max))
;;   (toggle-read-only)
;;   )
;; (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(defun my-shell-mode-hook ()
  "A hook to setup shell-mode."
  (setq comint-output-filter-functions
    (remove'ansi-color-process-output comint-output-filter-functions))
  (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)
  ;;; For ANSI colors
  ;; (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
  ;;; Removes face inherited from minibuffer
  (face-remap-set-base 'comint-highlight-prompt :inherit nil)
  ;;; You can highlight some text based on regexp (useful to see "OK" or warnings):
  ;; (add-hook 'shell-mode-hook (lambda () (highlight-regexp "\\[OK\\]" "hi-green-b")))
  ;;; Make URLs clickable
  (goto-address-mode)
  ;;; Make file paths clickable
  ;;; Every line representing a path to a file will be colorized and made clickable, so that you can jump to that file and that line, like in compilation-mode (specially useful when compiling a program or running tests):
  (compilation-shell-minor-mode)

  ;;; For some reasons must be in a hook
  (setq comint-input-ring-file-name "~/.config/zsh/.zhistory")
                                    ; Ignore timestamps in history file.  Assumes that zsh
                                    ; EXTENDED_HISTORY option is in use.
  (setq comint-input-ring-separator "\n: \\([0-9]+\\):\\([0-9]+\\);")

  (comint-read-input-ring nil)
  )
(add-hook 'shell-mode-hook 'my-shell-mode-hook)

;; Replaces RET behavior
(defun my-comint-send-input-maybe ()
  "Only `comint-send-input' when point is after the latest prompt.
Otherwise move to the end of the buffer.
TODO: Fix if current position is not prompt and does not have a previsou prompt"
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))) (cur (line-number-at-pos)))
    (if (and proc (>= (point) (marker-position (process-mark proc))))
        (comint-send-input)
        (comint-previous-prompt 1)
        (if (= cur (line-number-at-pos))
            (comint-copy-old-input)
            (comint-next-prompt 1)
            (if (= cur (line-number-at-pos))
                (comint-copy-old-input)
                (goto-char (point-max))
                )
            )
        )
    )
  )
(define-key shell-mode-map [remap comint-send-input] 'my-comint-send-input-maybe)

;;; shell history.
(define-key shell-mode-map (kbd "C-r") 'helm-comint-input-ring)
;; (define-key shell-mode-map (kbd "C-r") 'counsel-shell-history)

(defun my-comint-run-last-command(arg)
  "Run last command in shell mode. ARG is number."
  (interactive "*p")
  (let ((proc (get-buffer-process (current-buffer))))
    (unless (and proc (>= (point) (marker-position (process-mark proc))))
            (goto-char (point-max))
            )
            (comint-previous-input arg)
            (comint-send-input)
    )
  )

(defun my-run-last-command-in-shell-mode(arg)
  (interactive "p")
  (let ((cur (current-buffer)) (new) (buf) (list (buffer-list)))
    (while (and list (not new))
      (when (eq (buffer-local-value 'major-mode (car list)) 'shell-mode)
        (setq new (car list)))
      (setq list (cdr list))
      )
    (if new
        (progn
          (switch-to-buffer new t t)
          (my-comint-run-last-command arg)
          (switch-to-buffer cur t t)
          )
        (my-shell)
        )
    )
  )

(evil-define-key 'normal 'global (kbd "C-r") #'my-run-last-command-in-shell-mode)
(evil-define-key 'visual 'global (kbd "C-r") #'my-run-last-command-in-shell-mode)
(evil-define-key 'insert 'global (kbd "C-r") #'my-run-last-command-in-shell-mode)

(evil-define-key 'normal comint-mode-map
  (kbd "C-p") #'my-comint-run-last-command
  (kbd "}") #'comint-previous-prompt
  (kbd "{") #'comint-next-prompt
  (kbd "C-k") #'comint-previous-input
  (kbd "C-j") #'comint-next-input
  (kbd "M-k") #'comint-previous-input
  (kbd "M-j") #'comint-next-input
  (kbd "gk") #'comint-previous-input
  (kbd "gj") #'comint-next-input
  (kbd "<up>") #'comint-next-input
  (kbd "<down>") #'comint-previous-input
  )
(evil-define-key 'insert comint-mode-map
  (kbd "C-p") #'my-comint-run-last-command
  (kbd "C-k") #'comint-previous-input
  (kbd "C-j") #'comint-next-input
  (kbd "M-k") #'comint-previous-input
  (kbd "M-j") #'comint-next-input
  (kbd "<up>") #'comint-previous-input
  (kbd "<down>") #'comint-next-input
  )
))

(provide 'shell-config)
;;; shell-config.el ends here
