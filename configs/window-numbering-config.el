;;; window-numbering --- Numbered window shortcuts
;;; Commentary:
;;; Code:
(use-package window-numbering
  :init
  (defvar window-numbering-keymap
    (let ((map (make-sparse-keymap))) map)
    "Keymap used in by `window-numbering-mode'.
     Zero it on purpose, to avoid default bindings."
    )
  :config
  (setq window-numbering-auto-assign-0-to-minibuffer nil)

  (evil-define-key 'motion my-intercept-mode-map
    (kbd "SPC 0") 'select-window-0
    (kbd "SPC 1") 'select-window-1
    (kbd "SPC 2") 'select-window-2
    (kbd "SPC 3") 'select-window-3
    (kbd "SPC 4") 'select-window-4
    (kbd "SPC 5") 'select-window-5
    (kbd "SPC 6") 'select-window-6
    (kbd "SPC 7") 'select-window-7
    (kbd "SPC 8") 'select-window-8
    (kbd "SPC 9") 'select-window-9)
  (window-numbering-mode t)

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

(provide 'window-numbering-config)
;;; window-numbering-config.el ends here
