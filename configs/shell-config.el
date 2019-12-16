;;; shell-config --- Configures eshell and shell modes
;;; Commentary:
;;; Code:
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
;;; For better shell directory tracking, used below
(require 'dirtrack)
;;; Amazing xterm-256color package (see a comment below)
;; (use-package xterm-color)

;; (modify-syntax-entry ?_ "w" shell-mode-syntax-table)
;; (modify-syntax-entry ?- "w" shell-mode-syntax-table)

;; (require 'ansi-color)

;;; `shell` uses (pop-to-buffer buffer) instead of (switch-to-buffer buffer)
;;; That fucks up windows when `shell` is called.
;;; https://github.com/syl20bnr/spacemacs/issues/6820#issuecomment-239665146
;; (push (cons "\\*shell\\*" display-buffer--same-window-action) display-buffer-alist)
;;; https://stackoverflow.com/questions/40301732/m-x-shell-open-shell-in-other-windows
(add-to-list 'display-buffer-alist `(,(regexp-quote "*shell") display-buffer-same-window))

(setq
  comint-prompt-read-only t
  ;;; Remember lots of previous commands in shell-mode
  comint-input-ring-size 100000
  comint-input-ignoredups t
  )

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

  ;;; idk seems does not well work with comint, some sequences are wrongly interpreted
  ;; (setq comint-output-filter-functions
  ;;   (remove'ansi-color-process-output comint-output-filter-functions))
  ;; (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)

  ;;; For ANSI colors
  ;; (ansi-color-for-comint-mode-on)
  ;;; Removes face inherited from minibuffer. FIXME: Do I need it?
  ;; (face-remap-set-base 'comint-highlight-prompt)
  ;;; You can highlight some text based on regexp (useful to see "OK" or warnings):
  ;; (add-hook 'shell-mode-hook (lambda () (highlight-regexp "\\[OK\\]" "hi-green-b")))
  ;;; Make URLs clickable
  (goto-address-mode)
  ;;; Make file paths clickable
  ;;; Every line representing a path to a file will be colorized and made clickable, so that you can jump to that file and that line, like in compilation-mode (specially useful when compiling a program or running tests):
  (compilation-shell-minor-mode)

  ;;; For some reasons must be in a hook
  (setq comint-input-ring-file-name "~/.cache/zsh/.zhistory")
                                    ; Ignore timestamps in history file.  Assumes that zsh
                                    ; EXTENDED_HISTORY option is in use.
  (setq comint-input-ring-separator "\n: \\([0-9]+\\):\\([0-9]+\\);")

  (comint-read-input-ring nil)

  ;;; Stop the usual shell directory tracking
  (shell-dirtrack-mode 0)
  (set-variable 'dirtrack-list '("[[:blank:][:cntrl:]]+[[:cntrl:]]\\[38;5;81m\\([^[:cntrl:]]+\\)[[:cntrl:]]\\[39m$" 1 nil))
  ;;; Enable alternative tracking strategy
  (dirtrack-mode 1)
  (add-hook 'comint-preoutput-filter-functions 'dirtrack nil t)

  ;;; popular pagers do not work in shell-mode
  (process-send-string (get-buffer-process (current-buffer))
                       "export PAGER=cat NODE_NO_READLINE=1\n")

  ;;; {{{ This is for fine tuning of the dirtrack regexp for the prompt
  ;; (add-hook 'comint-preoutput-filter-functions
  ;;           'dirtrack-filter-out-pwd-prompt nil t)
  ;; (defun dirtrack-filter-out-pwd-prompt (string)
  ;;   (print (format "!!!|%s|!!!" string))
  ;;   (if (and (stringp string) (string-match (first dirtrack-list) string))
  ;;     (print (format "@@@|%s|@@@" (match-string 1 string))))
  ;;   string)
  ;;; }}}
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
  "Run last command in shell mode. ARG is a number."
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
  "Run last command in the last shell-buffer. ARG is a number."
  (interactive "p")
  (save-buffer)
  (let ((current-window (frame-selected-window)) (shell-buffer) (buf) (list (buffer-list)))
    (while (and list (not shell-buffer))
      (when (eq (buffer-local-value 'major-mode (car list)) 'shell-mode)
        (setq shell-buffer (car list)))
      (setq list (cdr list))
      )
    (if shell-buffer
        (progn
          (with-current-buffer shell-buffer (my-comint-run-last-command arg))
          (dolist (frame (frame-list))
            (dolist (win (window-list frame 0 nil))
              (when (eq shell-buffer (window-buffer win))
                    (select-window win t)
                    (goto-char (point-max))
                    )
              )
            )
          (select-window current-window t)
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
