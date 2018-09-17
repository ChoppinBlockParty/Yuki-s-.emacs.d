(setq-default fill-column 90)

(defun comment-auto-fill ()
      (setq-local comment-auto-fill-only-comments t)
      (auto-fill-mode 1))
(dolist (hook '(lisp-mode-hook
                emacs-lisp-mode-hook
                scheme-mode-hook
                clojure-mode-hook
                ruby-mode-hook
                yaml-mode
                python-mode-hook
                shell-mode-hook
                php-mode-hook
                css-mode-hook
                haskell-mode-hook
                caml-mode-hook
                c++-mode-hook
                c-mode-hook
                lua-mode-hook
                crontab-mode-hook
                perl-mode-hook
                tcl-mode-hook
                js2-mode-hook))
  (add-hook hook (lambda() (comment-auto-fill)))
)

(dolist (hook '(latex-mode-hook
                text-mode-hook))
  (add-hook hook 'turn-on-auto-fill))


(provide 'auto-fill-mode-config)
