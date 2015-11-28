
(use-package window-numbering
  ;; not deferred on puprose
  :ensure t
  :config
  (progn
    ;; (when (configuration-layer/package-usedp 'powerline)
    ;;   (defun window-numbering-install-mode-line (&optional position)
    ;;     "Do nothing, the display is handled by the powerline."))
    (setq window-numbering-auto-assign-0-to-minibuffer nil)
    (after 'evil
        (evil-leader/set-key
        "0" 'select-window-0
        "1" 'select-window-1
        "2" 'select-window-2
        "3" 'select-window-3
        "4" 'select-window-4
        "5" 'select-window-5
        "6" 'select-window-6
        "7" 'select-window-7
        "8" 'select-window-8
        "9" 'select-window-9)
        (window-numbering-mode 1)
    )

    ;; "Return the number of the window."
    ;; (let* ((num (window-numbering-get-number))
    ;;         (str (if num (int-to-string num))))
    ;; (cond
    ;;     ((not dotspacemacs-mode-line-unicode-symbols) str)
    ;;     ((equal str "1")  "➊")
    ;;     ((equal str "2")  "➋")
    ;;     ((equal str "3")  "➌")
    ;;     ((equal str "4")  "➍")
    ;;     ((equal str "5")  "➎")
    ;;     ((equal str "6")  "❻")
    ;;     ((equal str "7")  "➐")
    ;;     ((equal str "8")  "➑")
    ;;     ((equal str "9")  "➒")
    ;;     ((equal str "0")  "➓")))
  )
)

(provide 'window-numbering-config)