; (use-package cua
;   :ensure t
;   :config
  (progn
    (cua-mode t)
    ; (cua-enable-cua-keys nil)
    (setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
    (transient-mark-mode 1) ;; No region when it is not highlighted
    (setq cua-keep-region-after-copy t) ;; Standard Windows behaviour
    ;; Basic copy-paste setup. From wiki.
    (setq x-select-enable-clipboard t)
    (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
  )
; )

; Brilliant working copy-paste (even in Evil mode!) ripped from:
; http://hugoheden.wordpress.com/2009/03/08/copypaste-with-emacs-in-terminal/
(unless window-system
  (when (getenv "DISPLAY")
    ;; Callback for when user cuts
    (defun xsel-cut-function (text &optional push)
      ;; Insert text to temp-buffer, and "send" content to xsel stdin
      (with-temp-buffer
	(insert text)
	;; I prefer using the "clipboard" selection (the one the
	;; typically is used by c-c/c-v) before the primary selection
	;; (that uses mouse-select/middle-button-click)
	(call-process-region (point-min) (point-max) "xsel" nil 0 nil "--clipboard" "--input")))
    ;; Call back for when user pastes
    (defun xsel-paste-function()
      ;; Find out what is current selection by xsel. If it is different
      ;; from the top of the kill-ring (car kill-ring), then return
      ;; it. Else, nil is returned, so whatever is in the top of the
      ;; kill-ring will be used.
      (let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
	(unless (string= (car kill-ring) xsel-output)
	  xsel-output )))
    ;; Attach callbacks to hooks
    (setq interprogram-cut-function 'xsel-cut-function)
    (setq interprogram-paste-function 'xsel-paste-function)
    ;; Idea from
    ;; http://shreevatsa.wordpress.com/2006/10/22/emacs-copypaste-and-x/
    ;; http://www.mail-archive.com/help-gnu-emacs@gnu.org/msg03577.html
    ))

(after 'evil
  ; (defun paste-from-clipboard ()
  ;   (interactive)
  ;   (setq x-select-enable-clipboard t)
  ;   (yank)
  ;   (setq x-select-enable-clipboard nil))

  ; (defun copy-to-clipboard()
  ;   (interactive)
  ;   (setq x-select-enable-clipboard t)
  ;   (kill-ring-save (region-beginning) (region-end))
  ;   (setq x-select-enable-clipboard nil))

  ; (define-key evil-normal-state-map (kbd "C-c") 'copy-to-clipboard)
  ; (define-key evil-visual-state-map (kbd "C-c") 'copy-to-clipboard)
  ; (define-key evil-normal-state-map (kbd "C-x") 'paste-from-clipboard)
  ; (define-key evil-normal-state-map (kbd "C-v") 'paste-from-clipboard)
  ; (define-key evil-insert-state-map (kbd "C-v") 'paste-from-clipboard)

  (defun copy-to-clipboard ()
    (interactive)
    (if (display-graphic-p)
        (progn
          (message "Yanked region to x-clipboard!")
          (call-interactively 'clipboard-kill-ring-save)
          )
      (if (region-active-p)
          (progn
            (shell-command-on-region (region-beginning) (region-end) "xsel -i -b")
            (message "Yanked region to clipboard!")
            (deactivate-mark))
        (message "No region active; can't yank to clipboard!")))
    )

  (defun paste-from-clipboard ()
    (interactive)
    (if (display-graphic-p)
        (progn
          (clipboard-yank)
          (message "graphics active")
          )
      (insert (shell-command-to-string "xsel -o -b"))
      )
    )

  (define-key evil-normal-state-map (kbd "C-c") 'copy-to-clipboard)
  (define-key evil-visual-state-map (kbd "C-c") 'copy-to-clipboard)
  (define-key evil-visual-state-map (kbd "C-v") 'paste-from-clipboard)
  (define-key evil-normal-state-map (kbd "C-v") 'paste-from-clipboard)
  (define-key evil-insert-state-map (kbd "C-v") 'paste-from-clipboard)
)


(provide 'cua-config)
