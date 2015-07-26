(use-package sunrise-commander
  :ensure sunrise-commander
  :config
  (progn
    ; (defun my-configure-sinrise-commander ()
    ;   (sr-omit-mode 1))
    ; (add-hook 'dired-mode-hook 'my-configure-sinrise-commander)
    )
)

(after 'evil
  ;;; ============================================================================
  ;;; Sunrise Commander keybindings:
  ; (define-key evil-normal-state-map "`" 'sunrise)

  (define-key sr-mode-map "\C-m"        'sr-advertised-find-file)
  (define-key sr-mode-map "f"           'sr-advertised-find-file)
  (define-key sr-mode-map "X"           'sr-advertised-execute-file)
  (define-key sr-mode-map "o"           'sr-quick-view)
  (define-key sr-mode-map "v"           'sr-quick-view)
  (define-key sr-mode-map "/"           'sr-goto-dir)
  ; (define-key sr-mode-map "j"           'sr-goto-dir)
  (define-key sr-mode-map "^"           'sr-dired-prev-subdir)
  (define-key sr-mode-map "J"           'sr-dired-prev-subdir)
  (define-key sr-mode-map ";"           'sr-follow-file)
  (define-key sr-mode-map "\M-t"        'sr-transpose-panes)
  (define-key sr-mode-map "\M-o"        'sr-synchronize-panes)
  (define-key sr-mode-map "\C-\M-o"     'sr-project-path)
  (define-key sr-mode-map "\M-y"        'sr-history-prev)
  (define-key sr-mode-map "\M-u"        'sr-history-next)
  (define-key sr-mode-map "\C-c>"       'sr-checkpoint-save)
  (define-key sr-mode-map "\C-c."       'sr-checkpoint-restore)
  (define-key sr-mode-map "\C-c\C-z"    'sr-sync)
  (define-key sr-mode-map "\C-c\C-c"    'revert-buffer)

  (define-key sr-mode-map "\t"          'sr-change-window)
  (define-key sr-mode-map "\C-c\t"      'sr-select-viewer-window)
  (define-key sr-mode-map "\M-a"        'sr-beginning-of-buffer)
  (define-key sr-mode-map "\M-e"        'sr-end-of-buffer)
  (define-key sr-mode-map "\C-c\C-s"    'sr-split-toggle)
  (define-key sr-mode-map "]"           'sr-enlarge-left-pane)
  (define-key sr-mode-map "["           'sr-enlarge-right-pane)
  (define-key sr-mode-map "}"           'sr-enlarge-panes)
  (define-key sr-mode-map "{"           'sr-shrink-panes)
  (define-key sr-mode-map "\\"          'sr-lock-panes)
  (define-key sr-mode-map "\C-c}"       'sr-max-lock-panes)
  (define-key sr-mode-map "\C-c{"       'sr-min-lock-panes)
  (define-key sr-mode-map "\C-o"        'dired-omit-mode)
  (define-key sr-mode-map "b"           'sr-browse-file)
  (define-key sr-mode-map "\C-c\C-w"    'sr-browse-pane)
  (define-key sr-mode-map "\C-c\d"      'sr-toggle-attributes)
  (define-key sr-mode-map "\M-l"        'sr-toggle-truncate-lines)
  (define-key sr-mode-map "s"           'sr-interactive-sort)
  (define-key sr-mode-map "r"           'sr-reverse-pane)
  (define-key sr-mode-map "\C-e"        'sr-scroll-up)
  (define-key sr-mode-map "\C-y"        'sr-scroll-down)
  (define-key sr-mode-map " "           'sr-scroll-quick-view)
  (define-key sr-mode-map "\M- "        'sr-scroll-quick-view-down)
  (define-key sr-mode-map [?\S- ]       'sr-scroll-quick-view-down)

  (define-key sr-mode-map "C"           'sr-do-copy)
  (define-key sr-mode-map "K"           'sr-do-clone)
  (define-key sr-mode-map "R"           'sr-do-rename)
  (define-key sr-mode-map "D"           'sr-do-delete)
  (define-key sr-mode-map "x"           'sr-do-flagged-delete)
  (define-key sr-mode-map "S"           'sr-do-symlink)
  (define-key sr-mode-map "Y"           'sr-do-relsymlink)
  (define-key sr-mode-map "H"           'sr-do-hardlink)
  (define-key sr-mode-map "N"           'sr-inplace)
  (define-key sr-mode-map "\M-C"        'dired-do-copy)
  (define-key sr-mode-map "\M-R"        'dired-do-rename)
  (define-key sr-mode-map "\M-D"        'dired-do-delete)
  (define-key sr-mode-map "\M-S"        'dired-do-symlink)
  (define-key sr-mode-map "\M-Y"        'dired-do-relsymlink)
  (define-key sr-mode-map "\M-H"        'dired-do-hardlink)
  (define-key sr-mode-map "\C-x\C-q"    'sr-editable-pane)
  (define-key sr-mode-map "@"           'sr-fast-backup-files)
  (define-key sr-mode-map "\M-+"        'sr-create-files)

  (define-key sr-mode-map "="           'sr-diff)
  (define-key sr-mode-map "\C-c="       'sr-ediff)
  (define-key sr-mode-map "\C-x="       'sr-compare-panes)

  (define-key sr-mode-map "\C-c\C-f"    'sr-find)
  (define-key sr-mode-map "\C-c\C-n"    'sr-find-name)
  (define-key sr-mode-map "\C-c\C-g"    'sr-grep)
  (define-key sr-mode-map "\C-cb"       'sr-flatten-branch)
  (define-key sr-mode-map "\C-cp"       'sr-prune-paths)
  (define-key sr-mode-map "\C-c\C-l"    'sr-locate)
  (define-key sr-mode-map "\C-c/"       'sr-fuzzy-narrow)
  (define-key sr-mode-map "\C-c\C-r"    'sr-recent-files)
  (define-key sr-mode-map "\C-c\C-d"    'sr-recent-directories)
  (define-key sr-mode-map "\C-cv"       'sr-virtualize-pane)
  (define-key sr-mode-map "\C-c\C-v"    'sr-pure-virtual)
  (define-key sr-mode-map "Q"           'sr-do-query-replace-regexp)
  (define-key sr-mode-map "\C-q"        'sr-multi-occur)
  (define-key sr-mode-map "F"           'sr-do-find-marked-files)
  (define-key sr-mode-map "A"           'sr-do-search)
  (define-key sr-mode-map "\C-cs"       'sr-sticky-isearch-forward)
  (define-key sr-mode-map "\C-cr"       'sr-sticky-isearch-backward)
  (define-key sr-mode-map "\C-x\C-f"    'sr-find-file)
  (define-key sr-mode-map "y"           'sr-show-files-info)

  (evil-define-key 'normal sr-mode-map "j" 'sr-next-line-other)
  (define-key sr-mode-map [M-down]      'sr-next-line-other)
  (define-key sr-mode-map [A-down]      'sr-next-line-other)
  (define-key sr-mode-map "\M-p"        'sr-prev-line-other)
  (define-key sr-mode-map [M-up]        'sr-prev-line-other)
  (define-key sr-mode-map [A-up]        'sr-prev-line-other)
  (define-key sr-mode-map "\M-j"        'sr-goto-dir-other)
  (define-key sr-mode-map "\M-\C-m"     'sr-advertised-find-file-other)
  (define-key sr-mode-map "\M-f"        'sr-advertised-find-file-other)
  (define-key sr-mode-map "\C-c\C-m"    'sr-advertised-find-file-other)
  (define-key sr-mode-map "\M-^"        'sr-prev-subdir-other)
  (define-key sr-mode-map "\M-J"        'sr-prev-subdir-other)
  (define-key sr-mode-map "\M-m"        'sr-mark-other)
  (define-key sr-mode-map "\M-M"        'sr-unmark-backward-other)
  (define-key sr-mode-map "\M-U"        'sr-unmark-all-marks-other)
  (define-key sr-mode-map "\M-;"        'sr-follow-file-other)
  (define-key sr-mode-map "\C-\M-y"     'sr-history-prev-other)
  (define-key sr-mode-map "\C-\M-u"     'sr-history-next-other)

  (define-key sr-mode-map "\C-ct"       'sr-term)
  (define-key sr-mode-map "\C-cT"       'sr-term-cd)
  (define-key sr-mode-map "\C-c\C-t"    'sr-term-cd-newterm)
  (define-key sr-mode-map "\C-c\M-t"    'sr-term-cd-program)
  (define-key sr-mode-map "\C-c;"       'sr-follow-viewer)
  (define-key sr-mode-map "q"           'sr-quit)
  (define-key sr-mode-map "\C-xk"       'sr-kill-pane-buffer)
  (define-key sr-mode-map "\M-q"        'sunrise-cd)
  (define-key sr-mode-map "h"           'sr-describe-mode)
  (define-key sr-mode-map "?"           'sr-summary)
  (define-key sr-mode-map "k"           'dired-do-kill-lines)
  (define-key sr-mode-map [remap undo]  'sr-undo)
  (define-key sr-mode-map [remap undo-only] 'sr-undo)
  (define-key sr-mode-map [backspace]   'dired-unmark-backward)

  (define-key sr-mode-map [mouse-1]        'sr-mouse-advertised-find-file)
  (define-key sr-mode-map [mouse-2]        'sr-mouse-change-window)
  (define-key sr-mode-map [mouse-movement] 'sr-mouse-move-cursor)

  (define-key sr-mode-map [(control >)]         'sr-checkpoint-save)
  (define-key sr-mode-map [(control .)]         'sr-checkpoint-restore)
  (define-key sr-mode-map [(control tab)]       'sr-select-viewer-window)
  (define-key sr-mode-map [(control backspace)] 'sr-toggle-attributes)
  (define-key sr-mode-map [(control ?\=)]       'sr-ediff)
  (define-key sr-mode-map [(control meta ?\=)]  'sr-compare-panes)
  (define-key sr-mode-map [(control })]         'sr-max-lock-panes)
  (define-key sr-mode-map [(control {)]         'sr-min-lock-panes)

  (defvar sr-commander-keys
    '(([(f2)]            . sr-goto-dir)
      ([(f3)]            . sr-quick-view)
      ([(f4)]            . sr-advertised-find-file)
      ([(f5)]            . sr-do-copy)
      ([(f6)]            . sr-do-rename)
      ([(f7)]            . dired-create-directory)
      ([(f8)]            . sr-do-delete)
      ([(f10)]           . sr-quit)
      ([(control f3)]    . sr-sort-by-name)
      ([(control f4)]    . sr-sort-by-extension)
      ([(control f5)]    . sr-sort-by-time)
      ([(control f6)]    . sr-sort-by-size)
      ([(control f7)]    . sr-sort-by-number)
      ([(shift f7)]      . sr-do-symlink)
      ([(insert)]        . sr-mark-toggle)
      ([(control prior)] . sr-dired-prev-subdir))
    "Traditional commander-style keybindings for the Sunrise Commander.")

  (defcustom sr-use-commander-keys t
    "Whether to use traditional commander-style function keys (F5 = copy, etc)"
    :group 'sunrise
    :type 'boolean
    :set (defun sr-set-commander-keys (symbol value)
           "Setter function for the `sr-use-commander-keys' custom option."
           (if value
               (mapc (lambda (x)
                       (define-key sr-mode-map (car x) (cdr x))) sr-commander-keys)
             (mapc (lambda (x)
                     (define-key sr-mode-map (car x) nil)) sr-commander-keys))
           (set-default symbol value)))
)

(provide 'config-sunrise-commander)
