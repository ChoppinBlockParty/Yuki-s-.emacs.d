(use-package counsel
  :ensure t
  :after (flx)
  :init
  (progn

    ;; (defun +amos*ivy--insert-minibuffer (text)
  ;;   "Insert TEXT into minibuffer with appropriate cleanup."
  ;;   (let ((resize-mini-windows nil)
  ;;         (update-fn (ivy-state-update-fn ivy-last))
  ;;         (old-mark (marker-position (mark-marker)))
  ;;         deactivate-mark)
  ;;     (ivy--cleanup)
  ;;     (when update-fn (funcall update-fn))
  ;;     (ivy--insert-prompt)
  ;;     ;; Do nothing if while-no-input was aborted.
  ;;     (when (stringp text)
  ;;       (if ivy-display-function
  ;;           (funcall ivy-display-function text)
  ;;         (ivy-display-function-fallback text)))
  ;;     (unless (frame-root-window-p (minibuffer-window))
  ;;       (with-selected-window (minibuffer-window)
  ;;         (set-window-text-height nil
  ;;                                 (+ ivy-height
  ;;                                    (if ivy-add-newline-after-prompt
  ;;                                        1
  ;;                                      0)))))
  ;;     ;; prevent region growing due to text remove/add
  ;;     (when (region-active-p) (set-mark old-mark)))
  ;;   )
  ;; (advice-add #'ivy--insert-minibuffer :override #'+amos*ivy--insert-minibuffer)
  )
  :config
  (progn
    (ivy-mode t)
    (setq ivy-use-virtual-buffers t
          enable-recursive-minibuffers t
          ivy-display-style 'fancy
          ivy-count-format "(%d/%d) "
          ivy-re-builders-alist '((t . ivy--regex-fuzzy))
    ;; The ivy-initial-inputs-alist variable is pretty useful in conjunction with the default matcher. It's usually used to insert ^ into the input area for certain commands. If you're going fuzzy all the way, you can do without the initial ^, and simply let flx (hopefully) sort the matches in a nice way:
          ivy-initial-inputs-alist nil
          counsel-ag-base-command "ag --skip-vcs-ignores --ignore node_modules --ignore .git --ignore .build --ignore archive-contents --nocolor --nogroup %s"
          )

    (defun my-ivy-setup-minibuffer ()
      ""
      (let (map)
        (setq map ivy-minibuffer-map)
        ;; (evil-define-key 'normal map
        ;;   (kbd "<escape>") 'abort-recursive-edit
        ;;   (kbd "<return>") 'exit-minibuffer
        ;;   (kbd "C-m") 'ivy-done
        ;;   "j" 'ivy-next-line
        ;;   "k" 'ivy-previous-line)
        ;; (evil-define-key 'insert map
        ;;   [backspace] 'ivy-backward-delete-char
        ;;   (kbd "C-r") 'ivy-reverse-i-search
        ;;   (kbd "C-j") 'ivy-next-line
        ;;   (kbd "C-k") 'ivy-previous-line)
        ;;; C-m === RET
        (define-key map (kbd "C-m") 'ivy-done)
        (define-key map [down-mouse-1] 'ignore)
        (define-key map [mouse-1] 'ivy-mouse-done)
        (define-key map [mouse-3] 'ivy-mouse-dispatching-done)
        (define-key map (kbd "C-o") 'ivy-dispatching-done)
        ;; (define-key map (kbd "C-j") 'ivy-alt-done)
        (define-key map (kbd "C-u") 'ivy-call)
        (define-key map (kbd "C-M-u") 'ivy-immediate-done)
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
        (define-key map (kbd "C-r") 'ivy-reverse-i-search)
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
        (define-key map (kbd "C-g") 'minibuffer-keyboard-quit)
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
        ;; (define-key map (kbd "C-o") 'hydra-ivy/body)
        (define-key map [remap kill-line] 'ivy-kill-line)
        (define-key map [remap kill-whole-line] 'ivy-kill-whole-line)
        (define-key map (kbd "S-SPC") 'ivy-restrict-to-matches)
        (define-key map [remap kill-ring-save] 'ivy-kill-ring-save)
        (define-key map (kbd "C-'") 'ivy-avy)
        (define-key map (kbd "C-M-a") 'ivy-read-action)
        (define-key map (kbd "C-,") 'ivy-occur)
        (define-key map (kbd "C-c C-a") 'ivy-toggle-ignore)
        (define-key map (kbd "C-c C-s") 'ivy-rotate-sort)
        (define-key map [remap describe-mode] 'ivy-help)
        )
      )
    (my-ivy-setup-minibuffer)

    (defun evil-collection-ivy-setup ()
      "Set up `evil' bindings for `ivy-mode'."
      ;; (evil-set-initial-state 'ivy-occur-mode 'normal)
      (evil-set-initial-state 'ivy-occur-grep-mode 'normal)

      ;; (evil-define-key nil 'ivy-mode-map
      ;;   (kbd "<escape>") 'minibuffer-keyboard-quit)
      ;; (evil-define-key 'normal 'ivy-occur-mode-map
      ;;   [mouse-1] 'ivy-occur-click
      ;;   (kbd "<return>") 'ivy-occur-press-and-switch
      ;;   "j" 'ivy-occur-next-line
      ;;   "k" 'ivy-occur-previous-line
      ;;   "h" 'evil-backward-char
      ;;   "l" 'evil-forward-char
      ;;   "g" nil
      ;;   "gg" 'evil-goto-first-line
      ;;   "gf" 'ivy-occur-press
      ;;   "ga" 'ivy-occur-read-action
      ;;   "go" 'ivy-occur-dispatch
      ;;   "gc" 'ivy-occur-toggle-calling

      ;;   ;; refresh
      ;;   "gr" 'ivy-occur-revert-buffer

      ;;   (kbd "SPC w") 'kill-this-buffer
      ;;   ;; quit
      ;;   "q" 'quit-window)

      ;; (when evil-want-C-d-scroll
      ;;   (evil-define-key 'normal 'ivy-occur-grep-mode-map
      ;;     "D" 'ivy-occur-delete-candidate
      ;;     (kbd "C-d") 'evil-scroll-down))

      (evil-define-key 'visual ivy-occur-grep-mode-map
        "j" 'evil-next-line
        "k" 'evil-previous-line)

      (evil-define-key 'normal ivy-occur-grep-mode-map
        "d" 'ivy-occur-delete-candidate
        (kbd "C-x C-q") 'ivy-wgrep-change-to-wgrep-mode
        "i" 'ivy-wgrep-change-to-wgrep-mode
        "gd" 'ivy-occur-delete-candidate
        [mouse-1] 'ivy-occur-click
        (kbd "<return>") 'ivy-occur-press-and-switch
        "j" 'ivy-occur-next-line
        "k" 'ivy-occur-previous-line
        "h" 'evil-backward-char
        "l" 'evil-forward-char
        "g" nil
        "gg" 'evil-goto-first-line
        "gf" 'ivy-occur-press
        "gr" 'ivy-occur-revert-buffer
        "ga" 'ivy-occur-read-action
        "go" 'ivy-occur-dispatch
        "gc" 'ivy-occur-toggle-calling

        "0" 'evil-digit-argument-or-evil-beginning-of-line

        ;; "SPC" nil
        (kbd "SPC w") 'kill-buffer
        (kbd "SPC w") 'kill-this-buffer
        ;; quit
        "q" 'quit-window)
      )

     (evil-collection-ivy-setup)

    ))

(use-package ivy-rich
  :ensure t
  :after (all-the-icons counsel)
  :init
  (progn
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
               ((ivy-rich-switch-buffer-icon :width 2)
                (ivy-rich-candidate (:width 30))
                (ivy-rich-switch-buffer-size (:width 7))
                (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
                (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
                (ivy-rich-switch-buffer-project (:width 15 :face success))
                (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3)))))
                )
               :predicate (lambda (cand) (get-buffer cand))
               )
             )
          )

    (custom-set-faces
      '(ivy-minibuffer-match-face-1 ((t (:background "dark orchid" :foreground "#eeeeee" :weight bold))))
      '(ivy-minibuffer-match-face-2 ((t (:background "dark orchid" :foreground "#eeeeee" :weight bold))))
      '(ivy-minibuffer-match-face-3 ((t (:background "dark orchid" :foreground "#eeeeee" :weight bold))))
      '(ivy-minibuffer-match-face-4 ((t (:background "dark orchid" :foreground "#eeeeee" :weight bold))))
      )

    )
  :config
    (ivy-rich-mode t)
    )

(use-package counsel-projectile
  :ensure t
  :after(counsel)
  :config
  (progn
    )
  )


(provide 'ivy-config)
