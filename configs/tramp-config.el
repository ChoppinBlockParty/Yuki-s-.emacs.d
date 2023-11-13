;;; tramp-config --- Tramp configuration
;;; Commentary:
;;; Code:
(use-package tramp
  :init
  ;; (setq-default
  ;;   tramp-auto-save-directory nil
  ;;   tramp-use-ssh-controlmaster-options nil
  ;;   ;; -----------------------------------------------------------------------
  ;;   tramp-backup-directory-alist '((".*" . "~/.cache/emacs/tramp-backup"))
  ;;   ;; -----------------------------------------------------------------------
  ;;   tramp-completion-reread-directory-timeout nil
  ;;   ;; -----------------------------------------------------------------------
  ;;   ;; TODO:
  ;;   ;;
  ;;   ;; tramp-copy-size-limit
  ;;   ;; nil
  ;;   ;; -----------------------------------------------------------------------
  ;;   tramp-debug-buffer nil
  ;;   ;; -----------------------------------------------------------------------
  ;;   ;; -----------------------------------------------------------------------
  ;;   tramp-histfile-override "~/.tramp-history"
  ;;   ;; -----------------------------------------------------------------------
  ;;   ;; TODO:
  ;;   ;;
  ;;   ;; tramp-inline-compress-start-size
  ;;   ;; nil
  ;;   ;; -----------------------------------------------------------------------
  ;;   tramp-persistency-file-name "~/.cache/emacs/tramp-conn"
  ;;   ;; -----------------------------------------------------------------------
  ;;   tramp-remote-path '(tramp-default-remote-path
  ;;                       "/usr/local/sbin"
  ;;                       "/usr/local/bin"
  ;;                       "/usr/sbin"
  ;;                       "/usr/bin"
  ;;                       "/local/sbin"
  ;;                       "/local/bin"
  ;;                       "/sbin"
  ;;                       "/bin")
  ;;   ;; -----------------------------------------------------------------------
  ;;   tramp-verbose 10
  ;;   )
    ;; (setq-default tramp-shell-prompt-pattern
    ;;   "\\\\[\033[01;32m\]\H\[\033[00m\] \[\033[01;34m\]\w\[\033[00m\]\n\[\033[01;32m\]▶\[\033[00m\]")

  :config
    (setq tramp-mode nil)
  ;; (add-to-list 'tramp-methods
  ;;                '("bb"
  ;;                  (tramp-login-program      "ssh")
  ;;                  (tramp-login-args         (
  ;;                                             ("-l" "%u")
  ;;                                             ("-p" "%p")
  ;;                                             ("%c")
  ;;                                             ("-e" "none")
  ;;                                             ("-t")
  ;;                                             ("bb")
  ;;                                             ("\"")
  ;;                                             ("inline")
  ;;                                             ("%h")
  ;;                                             ("\"")
  ;;                                             ))
  ;;                  (tramp-remote-shell       "/bin/bash")
  ;;                  (tramp-remote-shell-args  ("-c"))
  ;;                  (tramp-remote-shell-login ("-l"))
  ;;                  (tramp-default-port       22)
  ;;                  (tramp-connection-timeout 30)))
  )

(provide 'tramp-config)
;;; tramp-config.el ends here
