

;; In order to work properly, we need to load evil-leader-mode before we load
;; evil-mode.
(use-package evil-leader
  :ensure evil-leader
  :demand evil-leader
  :config
  (progn
    (evil-leader/set-leader "<SPC>")
    (global-evil-leader-mode t)))

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
    (setq evil-normal-state-cursor   '("#f8f893" box))
    (setq evil-visual-state-cursor   '("orange" box))
    (setq evil-insert-state-cursor   '("#f8f893" bar))
    (setq evil-replace-state-cursor  '("#cc9393" box))
    (setq evil-operator-state-cursor '("red" hollow))
    (setq evil-want-fine-undo t)
    (setq evil-want-change-word-to-end t)

    (setq evil-auto-indent t)
    (setq evil-shift-width 2)
    (setq evil-regexp-search t)
    (setq evil-want-C-i-jump t)


    (use-package evil-nerd-commenter
      :ensure evil-nerd-commenter
      :commands (evilnc-comment-or-uncomment-lines))


    ; (use-package evil-matchit
    ;   :ensure evil-matchit
    ;   :commands evilmi-jump-items
    ;   :init
    ;   (progn
    ;     (setq global-evil-matchit-mode t)
    ;     (define-key evil-normal-state-map "%" 'evilmi-jump-items)))


    (use-package evil-surround
      :ensure evil-surround
      :config
      (progn
        (global-evil-surround-mode 1)))

    (evil-set-initial-state 'flycheck-error-list-mode 'normal)
    (evil-set-initial-state 'git-commit-mode 'insert)
    (evil-set-initial-state 'shell-mode 'emacs)
    (evil-set-initial-state 'esup-mode 'emacs)
    (evil-set-initial-state 'diff-mode 'emacs)
    (evil-set-initial-state 'term-mode 'emacs)
    (evil-set-initial-state 'multi-term-mode 'emacs)

    ; (use-package key-chord
    ;   :ensure key-chord
    ;   :diminish key-chord-mode
    ;   :config
    ;   (progn
    ;     (key-chord-mode 1)))

    (evil-define-text-object my-evil-next-match (count &optional beg end type)
      "Select next match."
      (evil-ex-search-previous 1)
      (evil-ex-search-next count)
      (list evil-ex-search-match-beg evil-ex-search-match-end))

    (evil-define-text-object my-evil-previous-match (count &optional beg end type)
      "Select previous match."
      (evil-ex-search-next 1)
      (evil-ex-search-previous count)
      (list evil-ex-search-match-beg evil-ex-search-match-end))


    (define-key evil-normal-state-map (kbd "\\") 'evil-window-vsplit)
    (define-key evil-normal-state-map (kbd "-") 'evil-window-split)

    (define-key evil-insert-state-map (kbd "RET") 'my-ret-and-indent)
    (define-key evil-insert-state-map (kbd "<S-backspace>")
      'backward-delete-char-untabify)
    (define-key evil-insert-state-map (kbd "<S-return>")
      'electric-indent-just-newline)
    (define-key evil-normal-state-map (kbd "<S-return>")
      'electric-indent-just-newline)

    (define-key evil-normal-state-map (kbd "SPC a") 'ag)
    (define-key evil-normal-state-map (kbd "SPC SPC") 'helm-M-x)

    (define-key evil-normal-state-map (kbd "C-q")   'universal-argument)

    ; (define-key evil-normal-state-map (kbd "C-h")   'evil-window-left)
    (define-key evil-normal-state-map (kbd "C-j")   'evil-window-down)
    (define-key evil-normal-state-map (kbd "C-k")   'evil-window-up)
    (define-key evil-normal-state-map (kbd "C-l")   'evil-window-right)

    (evil-leader/set-key "h" 'evil-window-left)
    (evil-leader/set-key "H" 'evil-window-move-far-left)
    (evil-leader/set-key "j" 'evil-window-down)
    (evil-leader/set-key "J" 'evil-window-move-very-bottom)
    (evil-leader/set-key "k" 'evil-window-up)
    (evil-leader/set-key "K" 'evil-window-move-very-top)
    (evil-leader/set-key "l" 'evil-window-right)
    (evil-leader/set-key "L" 'evil-window-move-far-right)

    (evil-leader/set-key "q" 'evil-window-delete)
    (define-key evil-normal-state-map "\C-\\" 'evil-window-delete)

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


    (define-key evil-normal-state-map "a"           'evil-append)
    (define-key evil-normal-state-map "A"           'my-electric-append-with-indent)
    ; (define-key evil-normal-state-map "$"           'my-smart-end)
    ; (define-key evil-normal-state-map "0"           'my-smart-home)

    (define-key evil-motion-state-map "h"           'evil-backward-char)
    (define-key evil-motion-state-map "j"           'evil-next-visual-line)
    (define-key evil-motion-state-map "k"           'evil-previous-visual-line)
    (define-key evil-motion-state-map "l"           'evil-forward-char)

    (define-key evil-normal-state-map "/"           'evil-search-forward)
    (define-key evil-normal-state-map (kbd "SPC /") 'helm-swoop)
    (define-key evil-motion-state-map "/"           'evil-search-forward)
    (define-key evil-normal-state-map (kbd "Y") (kbd "y$"))

    (evil-ex-define-cmd "Q"  'evil-quit)
    (evil-ex-define-cmd "Qa" 'evil-quit-all)
    (evil-ex-define-cmd "QA" 'evil-quit-all)

    (evil-define-key 'motion python-mode-map "]]" 'python-nav-forward-block)
    (evil-define-key 'motion python-mode-map "][" 'python-nav-end-of-block)
    (evil-define-key 'motion python-mode-map "[[" 'python-nav-backward-block)
    (evil-define-key 'motion python-mode-map "[]" 'my-python-nav-backward-end-of-block)
    (evil-define-key 'motion python-mode-map "[(" 'evil-previous-open-paren)
    (evil-define-key 'motion python-mode-map "])" 'evil-next-close-paren)
    (evil-define-key 'motion python-mode-map "[{" 'evil-previous-open-brace)
    (evil-define-key 'motion python-mode-map "]}" 'evil-next-close-brace)
    ))

(use-package evil-jumper
  :ensure evil-jumper
  :init
  ;; C-i and C-o don't work unless we load it again like this ...
  (require 'evil-jumper))

(provide 'configs-evil)
