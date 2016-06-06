;; In order to work properly, we need to load evil-leader-mode before we load
;; evil-mode.
(use-package evil-leader
  :ensure evil-leader
  :demand evil-leader
  :config
  (progn
    (evil-leader/set-leader "<SPC>")
    (global-evil-leader-mode t)))

; (use-package elscreen
;   :ensure t
;   :config
;   (progn
;   )
; )

; (use-package evil-tabs
;   :ensure t
;   :config
;   (progn
;     (global-evil-tabs-mode t)
;   )
; )

(use-package evil
  :ensure evil
  :config
  (progn

    (evil-mode 1)
    (setq evil-want-C-u-scroll t)
    (setq evil-want-C-w-in-emacs-state t)
    (setq evil-search-module         'isearch)
    (setq evil-magic                 'very-magic)
    (setq evil-emacs-state-cursor    '("#dfaf8f" box))
    (setq evil-normal-state-cursor   '("#00CED1" box))
    (setq evil-visual-state-cursor   '("orange" box))
    (setq evil-insert-state-cursor   '("#00CED1" bar))
    (setq evil-replace-state-cursor  '("#cc9393" box))
    (setq evil-operator-state-cursor '("red" hollow))
    (setq evil-want-fine-undo t)
    (setq evil-want-change-word-to-end t)

    (setq evil-auto-indent t)
    (setq evil-shift-width 2)
    (setq evil-regexp-search t)
    (setq evil-want-C-i-jump t)

    (use-package expand-region
      :ensure t
      :config
      (progn
        (define-key evil-motion-state-map (kbd "7") 'er/expand-region)
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
        (define-key evil-visual-state-map (kbd "gcc") 'comment-or-uncomment-region)
        ;(define-key evil-normarmal-state-map (kbd "cv") 'evilnc-toggle-invert-comment-line-by-line)
        ;(define-key evil-normarmal-state-map (kbd "\\") 'evilnc-comment-operator) ; if you prefer backslash key
        )
    )


    ; (use-package evil-matchit
    ;   :ensure evil-matchit
    ;   :commands evilmi-jump-items
    ;   :init
    ;   (progn
    ;     (setq global-evil-matchit-mode t)
    ;     (define-key evil-normal-state-map "%" 'evilmi-jump-items)))

;    (use-package evil-search-highlight-persist
;      :ensure t
;      :config
;      (progn
;        (global-evil-search-highlight-persist t)
;      )
;    )

    (use-package evil-surround
      :ensure evil-surround
      :config
      (progn
        (global-evil-surround-mode 1)))

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

    (define-key evil-insert-state-map (kbd "<S-backspace>")
      'backward-delete-char-untabify)

    (define-key evil-normal-state-map (kbd "SPC a") 'ag)
    (define-key evil-normal-state-map (kbd "SPC SPC") 'helm-M-x)

    (define-key evil-normal-state-map (kbd "C-u")   'universal-argument)

    (define-key evil-normal-state-map (kbd "C-h")   'evil-window-left)
    (define-key evil-normal-state-map (kbd "C-j")   'evil-window-down)
    (define-key evil-normal-state-map (kbd "C-k")   'evil-window-up)
    (define-key evil-normal-state-map (kbd "C-l")   'evil-window-right)

    (evil-leader/set-key "h" 'evil-window-left)
    (evil-leader/set-key "j" 'evil-window-down)
    (evil-leader/set-key "k" 'evil-window-up)
    (evil-leader/set-key "l" 'evil-window-right)

    (evil-leader/set-key "u" 'evil-window-delete)
    (define-key evil-normal-state-map "\C-\\" 'evil-window-delete)
    (define-key evil-normal-state-map "q" 'evil-window-delete)

    (defun capslock-digit-argument-fn (digit)
      `(lambda (arg)
         (interactive "P")
         (setq last-command-event (+ ,digit ?0))
         (digit-argument arg)))

    (define-key evil-normal-state-map "!" (capslock-digit-argument-fn 1))
    (define-key evil-normal-state-map "@" (capslock-digit-argument-fn 2))
    (define-key evil-normal-state-map "#" (capslock-digit-argument-fn 3))
    (define-key evil-normal-state-map "$" (capslock-digit-argument-fn 4))
    (define-key evil-normal-state-map "%" (capslock-digit-argument-fn 5))
    (define-key evil-normal-state-map "^" (capslock-digit-argument-fn 6))
    (define-key evil-normal-state-map "&" (capslock-digit-argument-fn 7))
    (define-key evil-normal-state-map "*" (capslock-digit-argument-fn 8))
    (define-key evil-normal-state-map "(" (capslock-digit-argument-fn 9))
    (define-key evil-normal-state-map ")" (capslock-digit-argument-fn 0))
    (define-key evil-normal-state-map (kbd "5") 'evil-beginning-of-line)
    (define-key evil-normal-state-map (kbd "8") 'evil-end-of-line)
    ;(define-key evil-normal-state-map (kbd "0") 'evil-search-highlight-persist-remove-all)
    (define-key evil-motion-state-map (kbd "3") 'evil-search-word-forward)
    (define-key evil-motion-state-map (kbd "4") 'evil-search-word-backward)

    (define-key evil-normal-state-map (kbd "9") 'evil-jump-item)

    (define-key evil-normal-state-map "a"           'evil-append)

    (define-key evil-motion-state-map "h"           'evil-backward-char)
    (define-key evil-motion-state-map "j"           'evil-next-visual-line)
    (define-key evil-motion-state-map "k"           'evil-previous-visual-line)
    (define-key evil-motion-state-map "l"           'evil-forward-char)
    (define-key evil-motion-state-map (kbd "M-u") 'evil-scroll-down)
    (define-key evil-motion-state-map (kbd "M-i") 'evil-scroll-up)

    (define-key evil-normal-state-map "/"           'evil-search-forward)
    (define-key evil-normal-state-map (kbd "SPC /") 'helm-swoop)
    (define-key evil-motion-state-map "/"           'evil-search-forward)

    (define-key evil-motion-state-map (kbd "C-s") 'evil-write)

    (define-key evil-motion-state-map (kbd "M-h") 'evil-jump-backward)
    (define-key evil-motion-state-map (kbd "M-l") 'evil-jump-forward)

    (define-key evil-motion-state-map (kbd "M-c") 'evil-visual-block)


    (define-key evil-motion-state-map (kbd ";") 'evil-ex)

    (define-key evil-normal-state-map "U" 'redo)
    (define-key evil-normal-state-map "Y" 'evil-join)
    (define-key evil-normal-state-map "t" 'evil-substitute)
    (define-key evil-normal-state-map "T" 'evil-change-whole-line)

    ;   ;; Set your own keyboard shortcuts to reload/save/switch WGs:
    ;   ;; "s" == "Super" or "Win"-key, "S" == Shift, "C" == Control
    ;   ; (define-key evil-normal-state-mode (kbd "<pause>")     'wg-reload-session)
    ;   ; (define-key evil-normal-state-mode (kbd "C-S-<pause>") 'wg-save-session)
    (define-key evil-normal-state-map (kbd "g t") 'elscreen-next)
    (define-key evil-normal-state-map (kbd "g T") 'elscreen-previous)
    ;   (workgroups-mode t)

    (evil-ex-define-cmd "Q"  'evil-quit)
    (evil-ex-define-cmd "Qa" 'evil-quit-all)
    (evil-ex-define-cmd "QA" 'evil-quit-all)

    (evil-define-key 'motion python-mode-map "]]" 'python-nav-forward-block)
    (evil-define-key 'motion python-mode-map "][" 'python-nav-end-of-block)
    (evil-define-key 'motion python-mode-map "[[" 'python-nav-backward-block)
    (evil-define-key 'motion python-mode-map "[(" 'evil-previous-open-paren)
    (evil-define-key 'motion python-mode-map "])" 'evil-next-close-paren)
    (evil-define-key 'motion python-mode-map "[{" 'evil-previous-open-brace)
    (evil-define-key 'motion python-mode-map "]}" 'evil-next-close-brace)



    ; Error in 24.5.1
    ;
    ; (use-package evil-jumper
    ; :ensure evil-jumper
    ; :init
    ; ;; C-i and C-o don't work unless we load it again like this ...
    ; (require 'evil-jumper))
  )
)

(provide 'configs-evil)
