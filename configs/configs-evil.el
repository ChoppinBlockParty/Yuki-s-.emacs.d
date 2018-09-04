;; In order to work properly, we need to load evil-leader-mode
;; before we load evil-mode.
(use-package evil-leader
  :ensure t
  :config
  (progn
    (evil-leader/set-leader "<SPC>")
    (global-evil-leader-mode t)))

(use-package evil
  :ensure t
  :config
  (progn
    (evil-mode t)
    (setq
      evil-want-C-u-scroll t
      evil-want-C-w-in-emacs-state t
      ; options:
      ; + evil-search - not native to Emacs
      ; + isearch     - native Emacs incremental search
      evil-search-module         'evil-search
      evil-magic                 'very-magic
      evil-emacs-state-cursor    '("#dfaf8f" box)
      evil-normal-state-cursor   '("#00CED1" box)
      evil-visual-state-cursor   '("orange" box)
      evil-insert-state-cursor   '("#00CED1" bar)
      evil-replace-state-cursor  '("#cc9393" box)
      evil-operator-state-cursor '("red" hollow)
      evil-want-fine-undo nil ; Googled it, people say it is buggy
      evil-want-change-word-to-end t
      evil-auto-indent t
      evil-shift-width 2
      evil-regexp-search t
      evil-want-C-i-jump t)

    (use-package expand-region
      :ensure t
      :config
      (progn
        ;; (define-key evil-motion-state-map (kbd "7") 'er/expand-symbol)
      )
    )


    (use-package evil-nerd-commenter
      :ensure t
      :config
      (progn
        ;; Emacs key bindings
        ; (global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)
        ; (global-set-key (kbd "C-c l") 'evilnc-quick-comment-or-uncomment-to-the-line)
        ; (global-set-key (kbd "C-c c") 'evilnc-copy-and-comment-lines)
        ; (global-set-key (kbd "C-c p") 'evilnc-comment-or-uncomment-paragraphs)

        ;; Vim key bindings
        (define-key evil-normal-state-map (kbd "gcc") 'evilnc-comment-or-uncomment-lines)
        ;(define-key evil-normal-state-map (kbd "cl") 'evilnc-quick-comment-or-uncomment-to-the-line)
        ;(define-key evil-normal-state-map (kbd "ll") 'evilnc-quick-comment-or-uncomment-to-the-line)
        ; (define-key evil-normal-state-map (kbd "cc") 'evilnc-copy-and-comment-lines)
        ; (define-key evil-norevil-normal-state-map (kbd "cp") 'evilnc-comment-or-uncomment-paragraphs)
        (define-key evil-visual-state-map (kbd "gc") 'comment-or-uncomment-region)
        ;(define-key evil-normarmal-state-map (kbd "cv") 'evilnc-toggle-invert-comment-line-by-line)
        ;(define-key evil-normarmal-state-map (kbd "\\") 'evilnc-comment-operator) ; if you prefer backslash key
        )
    )

    ;; (use-package evil-search-highlight-persist
    ;;   :ensure t
    ;;   :config
    ;;   (progn
    ;;     (global-evil-search-highlight-persist t)))

    (use-package evil-surround
      :ensure t
      :config
      (progn
        (global-evil-surround-mode t)))

    (evil-set-initial-state 'flycheck-error-list-mode 'normal)
    (evil-set-initial-state 'git-commit-mode 'insert)
    ;; (evil-set-initial-state 'shell-mode 'emacs)
    ;; (evil-set-initial-state 'esup-mode 'emacs)
    ;; (evil-set-initial-state 'diff-mode 'emacs)
    ;; (evil-set-initial-state 'term-mode 'emacs)
    ;; (evil-set-initial-state 'multi-term-mode 'emacs)

    (defun my-newline-without-break-of-line ()
        "1. remove to end of the line.
         2. insert newline with index"
        (interactive)
        (let ((oldpos (point)))
            (end-of-line)
            (newline-and-indent)))

    (define-key evil-normal-state-map (kbd "\\") 'evil-window-vsplit)
    (define-key evil-normal-state-map (kbd "-") 'evil-window-split)

    (define-key global-map (kbd "RET") 'newline-and-indent)
    (define-key evil-normal-state-map (kbd "RET") 'newline-and-indent)
    (define-key evil-insert-state-map (kbd "<S-return>") 'my-newline-without-break-of-line)
    (define-key evil-normal-state-map (kbd "<S-return>") 'my-newline-without-break-of-line)

    (define-key evil-insert-state-map (kbd "<S-backspace>") 'backward-delete-char-untabify)

    (define-key evil-normal-state-map (kbd "SPC a") 'ag)
    (define-key evil-normal-state-map (kbd "SPC SPC") 'helm-M-x)
    ;; Do not do this
    ;; (define-key evil-normal-state-map (kbd ":") 'helm-M-x)

    (define-key evil-normal-state-map (kbd "C-u")   'universal-argument)

    (evil-leader/set-key "h" 'evil-window-left)
    (evil-leader/set-key "j" 'evil-window-down)
    (evil-leader/set-key "k" 'evil-window-up)
    (evil-leader/set-key "l" 'evil-window-right)

    (define-key evil-normal-state-map "\C-\\" 'evil-window-delete)
    (define-key evil-normal-state-map "q" 'evil-window-delete)
    (with-eval-after-load 'with-editor
      (evil-define-key 'normal with-editor-mode-map "q" 'with-editor-finish))

    (defun capslock-digit-argument-fn (digit)
      `(lambda (arg)
         (interactive "P")
         (setq last-command-event (+ ,digit ?0))
         (digit-argument arg)))

    (define-key evil-motion-state-map "!" (capslock-digit-argument-fn 1))
    (define-key evil-motion-state-map "@" (capslock-digit-argument-fn 2))
    (define-key evil-motion-state-map "#" (capslock-digit-argument-fn 3))
    (define-key evil-motion-state-map "$" (capslock-digit-argument-fn 4))
    (define-key evil-motion-state-map "%" (capslock-digit-argument-fn 5))
    (define-key evil-motion-state-map "^" (capslock-digit-argument-fn 6))
    (define-key evil-motion-state-map "&" (capslock-digit-argument-fn 7))
    (define-key evil-motion-state-map "*" (capslock-digit-argument-fn 8))
    (define-key evil-motion-state-map "(" (capslock-digit-argument-fn 9))
    (define-key evil-motion-state-map ")" (capslock-digit-argument-fn 0))

    (evil-redirect-digit-argument evil-motion-state-map "0" 'evil-search-highlight-persist-remove-all)
    (define-key evil-normal-state-map "2" 'evil-record-macro)
    (define-key evil-motion-state-map (kbd "3") 'evil-search-word-forward)
    (define-key evil-motion-state-map (kbd "4") 'evil-search-word-backward)
    (define-key evil-motion-state-map (kbd "5") 'evil-beginning-of-line)
    (define-key evil-motion-state-map (kbd "8") 'evil-end-of-line)
    (define-key evil-motion-state-map (kbd "9") 'evil-jump-item)

    (define-key evil-normal-state-map "a"           'evil-append)

    (define-key evil-motion-state-map "h"           'evil-backward-char)
    (define-key evil-motion-state-map "j"           'evil-next-visual-line)
    (define-key evil-motion-state-map "k"           'evil-previous-visual-line)
    (define-key evil-motion-state-map "l"           'evil-forward-char)
    (define-key evil-motion-state-map (kbd "M-u") 'evil-scroll-down)
    (define-key evil-motion-state-map (kbd "M-i") 'evil-scroll-up)

    (define-key evil-motion-state-map "/"           'evil-search-forward)
    (define-key isearch-mode-map (kbd "<down>") 'isearch-ring-advance)
    (define-key isearch-mode-map (kbd "<up>")   'isearch-ring-retreat)
    (define-key isearch-mode-map (kbd "M-j")    'isearch-ring-advance)
    (define-key isearch-mode-map (kbd "M-k")    'isearch-ring-retreat)
    ;; (defun isearch-yank-symbol ()
    ;;   "*Put symbol at current point into search string."
    ;;   (interactive)
    ;;   (let ((sym (symbol-at-point)))
    ;;       (if sym
    ;;           (progn
    ;;           (setq isearch-regexp t
    ;;                 isearch-string (concat "\\_<" (regexp-quote (symbol-name sym)) "\\_>")
    ;;                 isearch-message (mapconcat 'isearch-text-char-description isearch-string "")
    ;;                 isearch-yank-flag t))
    ;;       (ding)))
    ;;   (isearch-search-and-update))
    ;; ;; (define-key evil-motion-state-map "7" 'isearch-yank-symbol)
    ;; (define-key evil-normal-state-map "7" 'evil-search-word-forward)

    (define-key evil-motion-state-map (kbd "C-s") 'evil-write)

    (define-key evil-motion-state-map (kbd "M-h") 'evil-jump-backward)
    (define-key evil-motion-state-map (kbd "M-l") 'evil-jump-forward)

    (define-key evil-motion-state-map (kbd "M-c") 'evil-visual-block)


    (define-key evil-motion-state-map (kbd ";") 'evil-ex)

    (define-key evil-normal-state-map (kbd "C-r") nil)
    (define-key evil-normal-state-map "U" 'redo)
    (define-key evil-normal-state-map "Y" 'evil-join)

    ;   ;; Set your own keyboard shortcuts to reload/save/switch WGs:
    ;   ;; "s" == "Super" or "Win"-key, "S" == Shift, "C" == Control
    ;   ; (define-key evil-normal-state-mode (kbd "<pause>")     'wg-reload-session)
    ;   ; (define-key evil-normal-state-mode (kbd "C-S-<pause>") 'wg-save-session)
    ;; (define-key evil-normal-state-map (kbd "g t") 'elscreen-next)
    ;; (define-key evil-normal-state-map (kbd "g T") 'elscreen-previous)
    ;   (workgroups-mode t)

    (evil-ex-define-cmd "Q"  'evil-quit)
    (evil-ex-define-cmd "Qa" 'evil-quit-all)
    (evil-ex-define-cmd "QA" 'evil-quit-all)

    ;; Marco related stuff
    ;; map("<Leader>r", "@:", Normal, Visual, Select, OperatorPending, NoRemap, Silent)
    ;; (define-key evil-normal-state-map "@" 'evil-execute-macro)
    ;; (define-key evil-normal-state-map "@" 'evil-execute-macro)
    (define-key evil-normal-state-map (kbd "SPC r")
      (lambda (count)
        (interactive
          (let (count)
              (setq count (if current-prefix-arg
                              (if (numberp current-prefix-arg)
                                  current-prefix-arg
                              0) 1)
              )
            (list count)))
        (evil-execute-macro count ?:)
      )
    )
    (define-key evil-normal-state-map (kbd "SPC e")
      (lambda (count)
        (interactive
          (let (count)
              (setq count (if current-prefix-arg
                              (if (numberp current-prefix-arg)
                                  current-prefix-arg
                              0) 1)
              )
            (list count)))
        (evil-execute-macro count ?a)
      )
    )
    (define-key evil-normal-state-map (kbd "SPC /") 'helm-swoop)
    (define-key evil-normal-state-map (kbd "SPC u g") 'magit-status)
    (define-key evil-normal-state-map (kbd "SPC u s") 'eshell)

    (use-package evil-easymotion
    :ensure t
    :config
    (progn
        ;; (setq avy-keys (number-sequence ?a ?z))

        (define-key evil-normal-state-map "J" nil)
        (evilem-define "J" #'evil-backward-WORD-begin)
        (evilem-define "K" #'evil-forward-WORD-begin)
        (evilem-define "H" #'evil-backward-WORD-end)
        (evilem-define "L" #'evil-forward-WORD-end)
        (evilem-define "t" #'evilem-motion-find-char-backward)
        (evilem-define "T" #'evilem-motion-find-char-to-backward)
        (evilem-define "f" #'evilem-motion-find-char)
        (evilem-define "F" #'evilem-motion-find-char-to)
        )
    )
  )
)

(provide 'configs-evil)
