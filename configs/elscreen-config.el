(use-package elscreen
  :ensure t
  :demand
  :config
  (progn
    (elscreen-start)
    (setq elscreen-tab-display-kill-screen 'right)

    (set-face-attribute 'elscreen-tab-control-face nil
                        :background "#000000"
                        :foreground "#1b1d1e")

    (set-face-attribute 'elscreen-tab-current-screen-face nil
                        :background "#465457"
                        :foreground "#882200")

    (set-face-attribute 'elscreen-tab-other-screen-face nil
                        :background "grey30"
                        :foreground "#1b1b1e")
    ; ;;(setq wg-session-load-on-start t)    ; default: (not (daemonp))

    ; ;; Change prefix key (before activating WG)
    ; ; (setq wg-prefix-key (kbd "C-c z"))

    ; ;; Change workgroups session file
    ; (setq wg-session-file "~/.emacs.d/.emacs_workgroups")


    (setq elscreen-display-tab nil) ; disable tabs display

    ;; get-alist was removed somewhere along the line
    ;; You can try substituting all instances of get-alist with assoc-default
    ;; instead of using defalias and see if that works; I haven't tried.
    (defalias 'get-alist 'assoc-default) ; get-alist is gone

    ;; Put tabs display in your frame title bar instead.
    (defun elscreen-frame-title-update ()
      (when (elscreen-screen-modified-p 'elscreen-frame-title-update)
        (let* ((screen-list (sort (elscreen-get-screen-list) '<))
               (screen-to-name-alist (elscreen-get-screen-to-name-alist))
               (title (concat "| " (mapconcat
                       (lambda (screen)
                         (format "%d%s %s |"
                                 screen (elscreen-status-label screen)
                                 (get-alist screen screen-to-name-alist)))
                       screen-list " "))))
          (if (fboundp 'set-frame-name)
              (set-frame-name title)
            (setq frame-title-format title)))))

    (eval-after-load "elscreen"
      '(add-hook 'elscreen-screen-update-hook 'elscreen-frame-title-update))
  )
)


(provide 'elscreen-config)
