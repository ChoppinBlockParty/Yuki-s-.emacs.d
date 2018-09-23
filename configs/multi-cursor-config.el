
(use-package evil-mc
  :after (flyspell)
  :init
  (defvar evil-mc-key-map
    (let ((map (make-sparse-keymap))
          (keys '(
                   ("TAB"   . my-evil-mc-make-all-cursors)
                   ("<tab>" . my-evil-mc-make-all-cursors)
                  )))
      (dolist (key-data keys)
        (evil-define-key 'normal map (kbd (car key-data)) (cdr key-data))
        (evil-define-key 'visual map (kbd (car key-data)) (cdr key-data)))
      map))
  :config
    (setq evil-mc-mode-line
      `(:eval
        (let ((mode-line-text-prefix "✐"))
          (if (> (evil-mc-get-cursor-count) 1)
              (evil-mc-active-mode-line mode-line-text-prefix)
            (when evil-mc-one-cursor-show-mode-line-text
              mode-line-text-prefix))))
      )

  (defun evil--mc-make-cursor-at-col (_startcol endcol orig-line)
    (move-to-column endcol)
    (unless (= (line-number-at-pos) orig-line)
      (evil-mc-make-cursor-here))
    )
  ;;; During visual selection point has +1 value
  (defun my-evil-mc-make-vertical-cursors (beg end)
    (interactive (list (region-beginning) (- (region-end) 1)))
    (evil-exit-visual-state)
    (evil-mc-pause-cursors)
    ;;; Because `evil-mc-resume-cursors` produces a cursor,
    ;;; we have to skip a current line here to avoid having +1 cursor
    (apply-on-rectangle #'evil--mc-make-cursor-at-col
                        beg end (line-number-at-pos))
    (evil-mc-resume-cursors)
    ;;; Because `evil-mc-resume-cursors` produces a cursor, we need to place it on on the
    ;;; same column as the others
    (move-to-column (evil-mc-column-number end))
    )
  (evil-define-key 'visual 'global "7" #'my-evil-mc-make-vertical-cursors)
  (evil-define-key 'normal 'global "7" #'my-evil-mc-make-all-cursors)
  (evil-define-command my-evil-mc-make-all-cursors ()
    "Initialize `evil-mc-pattern' and make cursors for all matches."
    :repeat ignore
    :evil-mc t
    (if (evil-mc-has-cursors-p)
          (progn
            (mapc 'evil-mc-delete-cursor evil-mc-cursor-list)
            (evil-exit-visual-state)
            (evil-mc-cursors-after))
      (evil-mc-set-pattern)
      (evil-exit-visual-state)
      (evil-mc-make-cursors-for-all)
      (evil-mc-print-cursors-info "Created")))
  (global-evil-mc-mode 1)
  )

(provide 'multi-cursor-config)
