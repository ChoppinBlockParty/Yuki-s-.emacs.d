(use-package golden-ratio
  :ensure t
  :demand
  :config
  (progn
    (golden-ratio-mode t)
    ;; (setq golden-ratio-auto-scale t)
    ;; (setq split-width-threshold nil)
    ;; (setq golden-ratio-exclude-modes '("ediff-mode"
                                       ;; "eshell-mode"
                                       ;; "dired-mode"))
    (setq golden-ratio-extra-commands
            (append golden-ratio-extra-commands
                    '(windmove-left
                      windmove-right
                      windmove-up
                      windmove-down
                      evil-window-delete
                      evil-window-split
                      evil-window-vsplit
                      evil-window-left
                      evil-window-right
                      evil-window-up
                      evil-window-down
                      evil-window-bottom-right
                      evil-window-top-left
                      evil-window-mru
                      evil-window-next
                      evil-window-prev
                      evil-window-new
                      evil-window-vnew
                      evil-window-rotate-upwards
                      evil-window-rotate-downwards
                      evil-window-move-very-top
                      evil-window-move-far-left
                      evil-window-move-far-right
                      evil-window-move-very-bottom
                      select-window-0
                      select-window-1
                      select-window-2
                      select-window-3
                      select-window-4
                      select-window-5
                      select-window-6
                      select-window-7
                      select-window-8
                      select-window-9
                      ace-jump-mode-pop-mark
                      buf-move-left
                      buf-move-right
                      buf-move-up
                      buf-move-down
                      ess-eval-buffer-and-go
                      ess-eval-function-and-go
                      ess-eval-line-and-go)))
   )
)

(provide 'golden-ratio-config)
