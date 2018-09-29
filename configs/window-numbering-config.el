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

  (evil-define-key 'normal my-intercept-mode-map
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
  (evil-define-key 'visual my-intercept-mode-map
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
  )

(provide 'window-numbering-config)
;;; window-numbering-config.el ends here
