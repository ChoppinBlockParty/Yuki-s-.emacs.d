;;; highlight-config --- summary
;;; Commentary:
;;; Code:

(use-package highlight-symbol
  :config
  (setq
    highlight-symbol-on-naviagtion-p t
    highlight-symbol-idle-delay 0
    )
  (global-set-key [f3] 'highlight-symbol)
  (global-set-key [(ctrl f3)] 'highlight-symbol-remove-all)
  (highlight-symbol-mode t)
  )

(use-package highlight-operators
  :ensure t
  :config
  ;; (defun enable-highlight-operators-mode()
  ;;   (highlight-operators-mode) nil)
  ;;   )
  (add-hook 'c++-mode-hook 'highlight-operators-mode)
  (add-hook 'sh-mode-hook 'highlight-operators-mode)
  (add-hook 'go-mode-hook 'highlight-operators-mode)
  (add-hook 'python-mode-hook 'highlight-operators-mode)
  (add-hook 'dockerfile-mode-hook 'highlight-operators-mode)
  )


;;; Highlights current line
;; (global-hl-line-mode t)
;; ;; To customize the background color
;; (set-face-background 'hl-line "#330")  ;; Emacs 22 Only

(provide 'highlight-config)
;;; highlight-config.el ends here
