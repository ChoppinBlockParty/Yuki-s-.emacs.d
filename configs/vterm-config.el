;;; vterm-config.el --- my VTerm configuration for Emacs
;;; Commentary:
;;; Code:
;;;

(use-package vterm
  :after (evil-config)
  :ensure t
  :init
    (setq vterm-always-compile-module t)
    (setq vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=no")
  :config
    (setq vterm-max-scrollback 70000)
    (setq vterm-buffer-name "*shell*")

    (defun my-vterm ()
      (interactive)
      (vterm vterm-buffer-name)
      )

    (defun my-vterm-run-last-command ()
      "Run last command in vterm mode."
      (interactive)
      (let ((proc (get-buffer-process (current-buffer))))
        (unless (and proc
                     (>= (point) (marker-position (process-mark proc))))
          (goto-char (point-max)))
        (vterm-send-key "<up>")
        (vterm-send-return)))

    (defun my-vtem-run-last-command-in-last-shell ()
      "Run the last command in the last vterm buffer."
      (interactive)
      (save-buffer)
      (let ((current-window (frame-selected-window))
            (shell-buffer)
            (buf)
            (list (buffer-list)))
          (while (and list (not shell-buffer))
            (when (eq (buffer-local-value 'major-mode (car list))
                      'vterm-mode)
              (setq shell-buffer (car list)))
            (setq list (cdr list)))
          (if shell-buffer
             (progn
               (with-current-buffer shell-buffer (my-vterm-run-last-command))
               (dolist (frame (frame-list))
                 (dolist (win (window-list frame 0 nil))
                   (when (eq shell-buffer (window-buffer win))
                     (select-window win t)
                     (goto-char (point-max)))))
               (select-window current-window t))
             (my-vterm))
          )
      )

    ;; evil-collection-vterm-setup {
    (add-hook 'vterm-mode-hook #'evil-collection-vterm-escape-stay)
    ;; Open to a better binding...
    (evil-collection-define-key '(normal insert) 'vterm-mode-map
        (kbd "C-c C-z") 'evil-collection-vterm-toggle-send-escape)
    ;; Evil has some "C-" bindings in insert state that shadow regular terminal
    ;; bindings. Don't raw-send "C-c" (prefix key) nor "C-h" (help prefix).
    (evil-collection-define-key 'insert 'vterm-mode-map
        (kbd "C-a") 'vterm--self-insert
        (kbd "C-b") 'vterm--self-insert     ; Should not be necessary.
        (kbd "C-d") 'vterm--self-insert
        (kbd "C-e") 'vterm--self-insert
        (kbd "C-f") 'vterm--self-insert     ; Should not be necessary.
        (kbd "C-k") 'vterm--self-insert
        (kbd "C-l") 'vterm--self-insert     ; Should not be necessary.
        (kbd "C-n") 'vterm--self-insert
        (kbd "C-o") 'vterm--self-insert
        (kbd "C-p") 'vterm--self-insert
        (kbd "C-q") 'vterm--self-insert     ; Should not be necessary.
        (kbd "C-r") 'vterm--self-insert
        (kbd "C-s") 'vterm--self-insert     ; Should not be necessary.
        (kbd "C-t") 'vterm--self-insert
        (kbd "C-u") 'vterm--self-insert     ; Should not be necessary.
        (kbd "C-v") 'vterm--self-insert     ; Should not be necessary.
        (kbd "C-w") 'vterm--self-insert
        (kbd "C-y") 'vterm--self-insert
        (kbd "C-z") 'vterm--self-insert
        (kbd "<delete>") 'vterm-send-delete)
    (evil-collection-define-key 'normal 'vterm-mode-map
        "}" 'vterm-previous-prompt
        "{" 'vterm-next-prompt
        "p" 'evil-collection-vterm-paste-after
        "P" 'vterm-yank
        "a" 'evil-collection-vterm-append
        "A" 'evil-collection-vterm-append-line
        "d" 'evil-collection-vterm-delete
        "D" 'evil-collection-vterm-delete-line
        "x" 'evil-collection-vterm-delete-char
        "X" 'evil-collection-vterm-delete-backward-char
        (kbd "RET") 'vterm-send-return
        "^" 'evil-collection-vterm-first-non-blank
        "i" 'evil-collection-vterm-insert
        "I" 'evil-collection-vterm-insert-line
        "u" 'vterm-undo
        "c" 'evil-collection-vterm-change
        "C" 'evil-collection-vterm-change-line
        "s" 'evil-collection-vterm-substitute
        "S" 'evil-collection-vterm-substitute-line)
    (evil-collection-define-key 'visual 'vterm-mode-map
        "d" 'evil-collection-vterm-delete
        "x" 'evil-collection-vterm-delete-backward-char)
    (evil-collection-define-key '(normal insert) 'vterm-mode-map
        (kbd "C-p") 'my-vterm-run-last-command)
    ;; evil-collection-vterm-setup }

    (evil-collection-define-key '(normal insert) 'vterm-mode-map
      (kbd "C-c") 'vterm--self-insert)

    (defun my-counsel-switch-to-shell-buffer ()
        "Switch to a shell buffer, or create one."
        (interactive)
        (ivy-read "Shell buffer: " (counsel--buffers-with-mode #'vterm-mode)
                    :action #'counsel--switch-to-shell
                    :caller 'counsel-switch-to-shell-buffer))
    (my-evil-all-modes-define-key "M-1" 'my-counsel-switch-to-shell-buffer)
  )

(provide 'vterm-config)
;;; vterm-config.el ends here
