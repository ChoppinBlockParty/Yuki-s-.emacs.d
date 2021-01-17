;;; ivy-config --- ivy
;;; Commentary:
;;; Code:
(use-package counsel
  :config
  (setq
    ivy-use-virtual-buffers t
    enable-recursive-minibuffers t
    ivy-do-completion-in-region nil
    ivy-extra-directories nil
    ivy-display-style 'fancy
    ivy-count-format "(%d/%d) "
    ivy-re-builders-alist '(
      (counsel-ag . ivy--regex-plus)
      (counsel-grep . ivy--regex-plus)
      (counsel-git-grep . ivy--regex-plus)
      (counsel-rg . ivy--regex-plus)
      (swiper . ivy--regex-plus)
      (t . ivy--regex-fuzzy)
      )
    ;;; ivy-initial-inputs-alist variable is pretty useful in
    ;;; conjunction with the default matcher. It's usually used to
    ;;; insert ^ into the input area for certain commands. If you're
    ;;; going fuzzy all the way, you can do without the initial ^,
    ;;; and simply let flx (hopefully) sort the matches in a nice
    ;;; way:
    ivy-initial-inputs-alist nil
    counsel-ag-base-command "ag --skip-vcs-ignores --hidden --ignore node_modules --ignore .git --ignore .build --ignore archive-contents --nocolor --nogroup %s"
    counsel-rg-base-command "rg -S --no-ignore-global --no-ignore-vcs --hidden --no-heading --line-number --color never %s ."
    )

  (let ((minor (assq 'ivy-mode minor-mode-alist)))
   (when minor (setcdr minor (list ""))))

  (modify-syntax-entry ?_ "w" ivy-occur-mode-syntax-table)
  (modify-syntax-entry ?_ "w" ivy-occur-grep-mode-syntax-table)

  (let ((map ivy-minibuffer-map))
    ;;; C-m === RET
    (define-key map (kbd "C-m") 'ivy-done)
    (define-key map [down-mouse-1] 'ignore)
    (define-key map [mouse-1] 'ivy-mouse-done)
    (define-key map [mouse-3] 'ivy-mouse-dispatching-done)
    (define-key map (kbd "C-g") 'ivy-dispatching-done)
    (define-key map (kbd "C-i") 'ivy-read-action)
    (define-key map (kbd "C-u") 'ivy-call)
    (define-key map (kbd "C-o") 'ivy-immediate-done)
    ;; Complete the minibuffer text as much as possible.
    (define-key map (kbd "TAB") 'ivy-partial-or-done)
    (define-key map [remap next-line] 'ivy-next-line)
    (define-key map [remap previous-line] 'ivy-previous-line)
    (define-key map (kbd "<up>") 'ivy-previous-line)
    (define-key map (kbd "<down>") 'ivy-next-line)
    (define-key map (kbd "C-k") 'ivy-previous-line)
    (define-key map (kbd "C-j") 'ivy-next-line)
    (define-key map (kbd "M-k") 'ivy-previous-line)
    (define-key map (kbd "M-j") 'ivy-next-line)
    (define-key map (kbd "C-s") 'ivy-next-line-or-history)
    ;;; Enter a recursive ‘ivy-read’ session using the current history.
    ;; (define-key map (kbd "C-r") 'ivy-reverse-i-search)
    (define-key map (kbd "SPC") 'self-insert-command)
    (define-key map [remap delete-backward-char] 'ivy-backward-delete-char)
    (define-key map [remap backward-delete-char-untabify] 'ivy-backward-delete-char)
    (define-key map [remap backward-kill-word] 'ivy-backward-kill-word)
    (define-key map (kbd "C-w") 'ivy-backward-kill-word)
    (define-key map [remap delete-char] 'ivy-delete-char)
    (define-key map [remap forward-char] 'ivy-forward-char)
    (define-key map (kbd "C-h") 'backward-char)
    (define-key map (kbd "M-h") 'backward-char)
    (define-key map (kbd "<right>") 'ivy-forward-char)
    (define-key map (kbd "C-l") 'ivy-forward-char)
    (define-key map (kbd "M-l") 'ivy-forward-char)
    (define-key map [remap kill-word] 'ivy-kill-word)
    (define-key map [remap beginning-of-buffer] 'ivy-beginning-of-buffer)
    (define-key map [remap end-of-buffer] 'ivy-end-of-buffer)
    (define-key map (kbd "C-p") 'ivy-previous-history-element)
    (define-key map (kbd "C-n") 'ivy-next-history-element)
    (define-key map (kbd "M-p") 'ivy-previous-history-element)
    (define-key map (kbd "M-n") 'ivy-next-history-element)
    (define-key map (kbd "<escape>") 'minibuffer-keyboard-quit)
    (define-key map [remap scroll-up-command] 'ivy-scroll-up-command)
    (define-key map [remap scroll-down-command] 'ivy-scroll-down-command)
    (define-key map (kbd "<next>") 'ivy-scroll-up-command)
    (define-key map (kbd "<prior>") 'ivy-scroll-down-command)
    (define-key map (kbd "C-v") 'ivy-scroll-up-command)
    (define-key map (kbd "M-v") 'ivy-scroll-down-command)
    (define-key map (kbd "C-M-n") 'ivy-next-line-and-call)
    (define-key map (kbd "C-M-p") 'ivy-previous-line-and-call)
    (define-key map (kbd "M-r") 'ivy-toggle-regexp-quote)
    ;; (define-key map (kbd "C-y") 'ivy-insert-current)
    (define-key map (kbd "C-y") 'ivy-yank-symbol)
    (define-key map (kbd "C-r") 'evil-paste-from-register)
    (define-key map [remap kill-line] 'ivy-kill-line)
    (define-key map [remap kill-whole-line] 'ivy-kill-whole-line)
    (define-key map (kbd "S-SPC") 'ivy-restrict-to-matches)
    (define-key map [remap kill-ring-save] 'ivy-kill-ring-save)
    (define-key map (kbd "C-'") 'ivy-avy)
    (define-key map (kbd "C-,") 'ivy-occur)
    (define-key map (kbd "C-c C-a") 'ivy-toggle-ignore)
    (define-key map (kbd "C-c C-s") 'ivy-rotate-sort)
    (define-key map [remap describe-mode] 'ivy-help)
    )

  (evil-set-initial-state 'ivy-occur-grep-mode 'normal)

  (evil-define-key 'visual ivy-occur-grep-mode-map
    "j" 'evil-next-line
    "k" 'evil-previous-line
    )

  (evil-define-key 'normal ivy-occur-grep-mode-map
    "d" 'ivy-occur-delete-candidate
    ;; (kbd "C-x C-q") 'ivy-wgrep-change-to-wgrep-mode
    "i" 'ivy-wgrep-change-to-wgrep-mode
    [mouse-1] 'ivy-occur-click
    (kbd "<return>") 'ivy-occur-press-and-switch
    "j" 'ivy-occur-next-line
    "k" 'ivy-occur-previous-line
    "h" 'evil-backward-char
    "l" 'evil-forward-char
    "gr" 'ivy-occur-revert-buffer
    "ga" 'ivy-occur-read-action
    "go" 'ivy-occur-dispatch
    "zf" 'ivy-occur-toggle-calling

    "0" 'evil-digit-argument-or-evil-beginning-of-line

    ;; "SPC" nil
    (kbd "SPC w") 'kill-buffer
    (kbd "SPC w") 'kill-this-buffer
    ;; quit
    "q" 'quit-window
    )

  (ivy-mode t)
  )

(use-package ivy-rich
  :config
  (defun ivy-rich-switch-buffer-icon (candidate)
    (with-current-buffer
      (get-buffer candidate)
      (let ((icon (all-the-icons-icon-for-mode major-mode)))
              (if (symbolp icon)
                  (all-the-icons-icon-for-mode 'fundamental-mode)
                icon)
        )
      )
    )
  (setq ivy-rich--display-transformers-list
        '(ivy-switch-buffer
           (:columns
             ((ivy-rich-candidate (:width 30))
              (ivy-rich-switch-buffer-size (:width 7))
              (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
              (ivy-rich-switch-buffer-icon :width 2)
              (ivy-rich-switch-buffer-project (:width 15 :face success))
              (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3)))))
              )
             :predicate (lambda (cand) (get-buffer cand))
             )
           )
        )
  (ivy-rich-mode t)
  )

(use-package counsel-projectile)

(provide 'ivy-config)
;;; ivy-config.el ends here
