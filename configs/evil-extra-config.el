;;; evil-extra-config --- Extra config for evil
;;; Commentary:
;;; Code:

(use-package evil-nerd-commenter
  :config
  (dolist (hook (append my-prog-modes-hook-list my-markup-modes-hook-list))
    (add-hook hook (lambda()
      (define-key evil-normal-state-map (kbd "gcc") 'evilnc-comment-or-uncomment-lines)
      (define-key evil-visual-state-map (kbd "gc") 'comment-or-uncomment-region)
      ))
    )
  )

(use-package evil-surround
  :config
  ;;; Add surrounding
  ;;; You can surround in visual-state with S<textobject> or gS<textobject>. Or in
  ;;; normal-state with ys<textobject> or yS<textobject>.
  ;;; Change surrounding
  ;;; You can change a surrounding with cs<old-textobject><new-textobject>.
  ;;; Delete surrounding
  ;;; You can delete a surrounding with ds<textobject>.
  (global-evil-surround-mode t)
  (setq-default evil-surround-mode t)
  )

;;; Match everything
;;; https://github.com/redguardtoo/evil-matchit
;;; Do not use anymore - has a lot of bugs
;; (use-package evil-matchit
;;   :config
;;   (defun evilmi-customize-keybinding()
;;     (evil-define-key 'normal evil-matchit-mode-map "9" 'evilmi-jump-items)
;;     (evil-define-key 'visual evil-matchit-mode-map "9" 'evilmi-jump-items)
;;     )
;;   (global-evil-matchit-mode 1)
;;   )

(provide 'evil-extra-config)
;;; evil-extra-config.el ends here
