;;; evil-config --- evil it is
;;; Commentary:
;;; Code:

(use-package evil
  :ensure nil
  :load-path "local/evil"
  :config
  (evil-mode t)
  ;;; options:
  ;;; + `evil-search` - not native to Emacs
  ;;;   use `evil-ex-search-forward/backward`
  ;;; + `isearch`     - native Emacs incremental search
  ;;;   use `evil-search-forward/backward`
  (evil-select-search-module 'evil-search-module 'evil-search)

;;; In order to work properly, we need to load evil-leader-mode
;;; before we load evil-mode.
(use-package evil-leader
  :config
  (evil-leader/set-leader "<SPC>")
  (global-evil-leader-mode t)
  )

  ;; TODO: Highlight in all windows

  (defun get-hl-buffers()
    (let ((bufs (list)) buf)
      (dolist (win (window-list nil -1 nil))
        (setq buf (window-buffer win))
        (unless (memq buf bufs)
          (setq bufs (append bufs (list buf))))
        )
      bufs))

(defun evil-ex-hl-update-highlights ()
  "Update the overlays of all active highlights."
  (dolist (hl (mapcar #'cdr evil-ex-active-highlights-alist))
    (let* ((old-ovs (evil-ex-hl-overlays hl))
           new-ovs
           (pattern (evil-ex-hl-pattern hl))
           (case-fold-search (evil-ex-pattern-ignore-case pattern))
           (case-replace case-fold-search)
           (face (evil-ex-hl-face hl))
           (match-hook (evil-ex-hl-match-hook hl))
           result)
      (if pattern
          ;; collect all visible ranges
          (let (ranges sranges)
            (dolist (win (if (eq evil-ex-interactive-search-highlight
                                 'all-windows)
                             (get-buffer-window-list (current-buffer) nil t)
                           (list (evil-ex-hl-window hl))))
              (let ((beg (max (window-start win)
                              (or (evil-ex-hl-min hl) (point-min))))
                    (end (min (window-end win t)
                              (or (evil-ex-hl-max hl) (point-max)))))
                (when (< beg end)
                  (push (cons beg end) ranges))))
            (setq ranges
                  (sort ranges #'(lambda (r1 r2) (< (car r1) (car r2)))))
            (while ranges
              (let ((r1 (pop ranges))
                    (r2 (pop ranges)))
                (cond
                 ;; last range
                 ((null r2)
                  (push r1 sranges))
                 ;; ranges overlap, union
                 ((>= (cdr r1) (car r2))
                  (push (cons (car r1)
                              (max (cdr r1) (cdr r2)))
                        ranges))
                 ;; ranges distinct
                 (t
                  (push r1 sranges)
                  (push r2 ranges)))))

            ;; run through all ranges
            (condition-case lossage
                (save-match-data
                  (dolist (r sranges)
                    (let ((beg (car r))
                          (end (cdr r)))
                      (save-excursion
                        (goto-char beg)
                        ;; set the overlays for the current highlight,
                        ;; reusing old overlays (if possible)
                        (while (and (not (eobp))
                                    (evil-ex-search-find-next-pattern pattern)
                                    (<= (match-end 0) end)
                                    (not (and (= (match-end 0) end)
                                              (string= (evil-ex-pattern-regex pattern)
                                                       "^"))))
                          (let ((ov (or (pop old-ovs) (make-overlay 0 0))))
                            (move-overlay ov (match-beginning 0) (match-end 0))
                            (overlay-put ov 'face face)
                            (overlay-put ov 'evil-ex-hl (evil-ex-hl-name hl))
                            (overlay-put ov 'priority 1000)
                            (push ov new-ovs)
                            (when match-hook (funcall match-hook hl ov)))
                          (cond
                           ((and (not (evil-ex-pattern-whole-line pattern))
                                 (not (string-match-p "\n" (buffer-substring-no-properties
                                                            (match-beginning 0)
                                                            (match-end 0)))))
                            (forward-line))
                           ((= (match-beginning 0) (match-end 0))
                            (forward-char))
                           (t (goto-char (match-end 0))))))))
                  (mapc #'delete-overlay old-ovs)
                  (evil-ex-hl-set-overlays hl new-ovs)
                  (if (or (null pattern) new-ovs)
                      (setq result t)
                    ;; Maybe the match could just not be found somewhere else?
                    (save-excursion
                      (goto-char (or (evil-ex-hl-min hl) (point-min)))
                      (if (and (evil-ex-search-find-next-pattern pattern)
                               (< (match-end 0) (or (evil-ex-hl-max hl)
                                                    (point-max))))
                          (setq result (format "Match in line %d"
                                               (line-number-at-pos
                                                (match-beginning 0))))
                        (setq result "No match")))))

              (invalid-regexp
               (setq result (cadr lossage)))

              (search-failed
               (setq result (nth 2 lossage)))

              (error
               (setq result (format "%s" (cadr lossage))))

              (user-error
               (setq result (format "%s" (cadr lossage))))))
        ;; no pattern, remove all highlights
        (mapc #'delete-overlay old-ovs)
        (evil-ex-hl-set-overlays hl new-ovs))
      (when (evil-ex-hl-update-hook hl)
        (funcall (evil-ex-hl-update-hook hl) hl result)))))

(defun evil-ex-search-activate-highlight (pattern)
  "Activate highlighting of the search pattern set to PATTERN.
This function does nothing if `evil-ex-search-interactive' or
`evil-ex-search-highlight-all' is nil. "
  (when (and evil-ex-search-interactive evil-ex-search-highlight-all)
    (dolist (buf (get-hl-buffers))
      (with-current-buffer buf
        (unless (evil-ex-hl-active-p 'evil-ex-search)
          (evil-ex-make-hl 'evil-ex-search :win nil))
        (when pattern (evil-ex-hl-change 'evil-ex-search pattern))
       )
      )))

(defun evil-ex-hl-idle-update ()
  "Triggers the timer to update the highlights in the current buffer."
  (when (and evil-ex-interactive-search-highlight
             evil-ex-active-highlights-alist)
    (evil-ex-hl-do-update-highlight (current-buffer))
    ))

(defun evil-ex-hl-change (name pattern)
  "Set the regular expression of highlight NAME to PATTERN."
  (let ((hl (cdr-safe (assoc name evil-ex-active-highlights-alist))))
    (when hl
      (evil-ex-hl-set-pattern hl (if (zerop (length pattern)) nil pattern))
      (evil-ex-hl-idle-update)
      )
    )
  )

;; (defun my-evil-ex-hl-do-update-highlight (beg end &optional buffer)
;;   "Timer function for updating the highlights."
;;   (when (buffer-live-p buffer)
;;     (with-current-buffer buffer
;;       (my-evil-ex-hl-update-highlights beg end)))
;;   (setq evil-ex-hl-update-timer nil))

;; (defun evil-ex-hl-update-highlights-scroll (win beg)
;;   "Update highlights after scrolling in some window."
;;   (with-current-buffer (window-buffer win)
;;     (my-evil-ex-hl-idle-update)))

  (setq
    evil-toggle-key "C-z"
    evil-echo-state nil
    evil-kbd-macro-suppress-motion-error t
    ;;; Whether \"C-i\" jumps forward like in Vim.
    evil-want-C-i-jump nil
    evil-want-C-u-scroll t
    evil-want-C-w-in-emacs-state t
    ;;; Whether \"cw\" behaves like \"ce\".
    evil-want-change-word-to-end nil
    evil-emacs-state-cursor    '("#dfaf8f" bar)
    evil-normal-state-cursor   '("#00ced1" box)
    evil-visual-state-cursor   '("orange" box)
    evil-insert-state-cursor   '("#00ced1" bar)
    evil-replace-state-cursor  '("#cc9393" box)
    evil-operator-state-cursor '("red" hollow)
    evil-want-fine-undo nil ; googled it, people say it is buggy
    evil-want-change-word-to-end t
    evil-auto-indent t
    ;;; Type of default ex range in visual char state.
    ;;; If non-nil the default range when starting an ex command from
    ;;; character visual state is `<,`> otherwise it is '<,'>. In the
    ;;; first case the ex command will be passed a region covering only
    ;;; the visual selection. In the second case the passed region will
    ;;; be extended to contain full lines.
    evil-ex-visual-char-range t
    evil-ex-substitute-case 'sensitive
    evil-ex-search-case 'smart
    ;;; Use of "\v" means that in the pattern after it all ASCII characters except '0'-'9', 'a'-'z', 'A'-'Z' and '_' have a special meaning.  "very magic"
    evil-magic 'very-magic
    ;;; Whether to use regular expressions for searching.
    evil-regexp-search t
    ;;; Updates search results as you are typing
    evil-ex-search-interactive t
    ;;; If t and substitute patterns are highlighted, the replacement is shown interactively.
    evil-ex-substitute-interactive-replace t
    ;;; If t and interactive search is enabled, all matches are highlighted.
    evil-ex-search-highlight-all t
    evil-ex-substitute-highlight-all t
    ;;; If non-nil matches remain highlighted when the search ends.
    evil-ex-search-persistent-highlight t
    ;;; Determine in which windows the interactive highlighting should be shown.
    evil-ex-interactive-search-highlight 'all-windows
    ;;; If non-nil substitute patterns are global by default. Usually (if this variable
    ;;; is nil) a substitution works only on the first match of a pattern in a line
    ;;; unless the 'g' flag is given, in which case the substitution happens on all
    ;;; matches in a line. If this option is non-nil, this behaviour is reversed: the
    ;;; substitution works on all matches unless the 'g' pattern is specified, then is
    ;;; works only on the first match.
    evil-ex-substitute-global t
    )
  (setq-default
    evil-shift-width 2
    ;;; If nil then * and # search for words otherwise for symbols.
    evil-symbol-word-search t
   )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Global
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defvar my-intercept-mode-map (make-sparse-keymap)
    "High precedence keymap.")

  (define-minor-mode my-intercept-mode
    "Global minor mode for higher precedence evil keybindings."
    :global t)

  (my-intercept-mode)

  (dolist (state '(normal visual insert))
    (evil-make-intercept-map
      ;; NOTE: This requires an evil version from 2018-03-20 or later
      (evil-get-auxiliary-keymap my-intercept-mode-map state t t)
      state))

  (defun my-global-define-key (key func)
    "Define globally in evil"
    (evil-define-key 'motion my-intercept-mode-map key func)
    (evil-define-key 'emacs my-intercept-mode-map key func)
    (evil-define-key 'insert my-intercept-mode-map key func)
    )
  (defun my-switch-to-previous-buffer ()
    "Switch to most recent buffer. Repeated calls toggle back and forth between the most recent two buffers."
    (interactive)
    (switch-to-buffer (other-buffer (current-buffer) 1))
    )
  (defun my-switch-to-previous-window ()
    (interactive)
    (select-window (previous-window))
    )
  (my-global-define-key (kbd "M-1") 'counsel-switch-to-shell-buffer)
  (my-global-define-key (kbd "M-2") 'my-switch-to-previous-window)
  (my-global-define-key (kbd "M-3") 'my-switch-to-previous-buffer)

  ;;; Only for testing emacs configuration, REMOVE ME
  (my-global-define-key (kbd "C-q") 'kill-emacs)
  (defun run-emacs ()
    "Usefull for testing Emacs configuration."
    (interactive)
    (start-process-shell-command "emacs" nil "emacs --debug-init ~/Others/Vim/Keybindings.py")
    )
  (my-global-define-key (kbd "C-1") 'run-emacs)

  (evil-define-key 'insert my-intercept-mode-map [f12] 'evil-command-window-ex)
  (evil-define-key 'visual my-intercept-mode-map [f12] 'evil-command-window-ex)
  (evil-define-key 'normal my-intercept-mode-map [f12] 'evil-command-window-ex)

  ;;; Do not do this
  ;;; (define-key evil-normal-state-map (kbd ":") 'helm-M-x)
  (define-key evil-normal-state-map (kbd "SPC a") 'ff-find-other-file)
  (define-key evil-normal-state-map (kbd "SPC w") 'kill-this-buffer)

  (evil-define-key 'normal 'global (kbd "SPC r") 'evil-ex-repeat)
  (evil-define-key 'visual 'global (kbd "SPC r") 'evil-ex-repeat-substitute)

  ;;; The code is almost a copy-paste from `evil-execute-macro
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
      (let ((macro (evil-get-register ?a t)))
        (cond
          ((or (and (not (stringp macro))
                (not (vectorp macro)))
                (member macro '("" [])))
             (user-error "No previous macro"))
          (t (condition-case err
               (evil-with-single-undo
                 (execute-kbd-macro macro count))
               (error
                (evil-normal-state)
                (evil-normalize-keymaps)
                (signal (car err) (cdr err)))
               ))
        ))
      )
    )

  ;;; R must be re-mapable in other modes
  (evil-define-key 'normal 'global (kbd "R")
    (lambda()
      (interactive)
      (let ((str (evil-find-thing nil 'symbol)))
        (evil-ex (format "s/\\<%s\\>/" (regexp-quote str)))
        )
      ))
  (evil-define-key 'visual 'global (kbd "R")
    (lambda(beg end)
      (interactive "r")
      (let ((str (evil-find-thing nil 'symbol)))
        (evil-ex (format "s/%s/" (regexp-quote (buffer-substring-no-properties beg end))))
        )
      ))

  (evil-set-initial-state 'flycheck-error-list-mode 'normal)
  (evil-set-initial-state 'git-commit-mode 'insert)

  (defun my-newline-without-break-of-line ()
    "1. remove to end of the line.
     2. insert newline with index"
    (interactive)
    (let ((oldpos (point)))
        (end-of-line)
        (newline-and-indent)))

  (evil-define-key 'normal my-intercept-mode-map (kbd "\\") 'evil-window-vsplit)
  (evil-define-key 'normal my-intercept-mode-map (kbd "-") 'evil-window-split)

  (define-key global-map (kbd "RET") 'newline-and-indent)
  (define-key evil-normal-state-map (kbd "RET") 'newline-and-indent)
  (define-key evil-insert-state-map (kbd "<S-return>") 'my-newline-without-break-of-line)
  (define-key evil-normal-state-map (kbd "<S-return>") 'my-newline-without-break-of-line)

  (define-key evil-insert-state-map (kbd "<S-backspace>") 'backward-delete-char-untabify)

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
  (define-key evil-normal-state-map "@" (capslock-digit-argument-fn 2))
  (define-key evil-motion-state-map "#" (capslock-digit-argument-fn 3))
  (define-key evil-motion-state-map "$" (capslock-digit-argument-fn 4))
  (define-key evil-motion-state-map "%" (capslock-digit-argument-fn 5))
  (define-key evil-motion-state-map "^" (capslock-digit-argument-fn 6))
  (define-key evil-normal-state-map "&" (capslock-digit-argument-fn 7))
  (define-key evil-motion-state-map "*" (capslock-digit-argument-fn 8))
  (define-key evil-motion-state-map "(" (capslock-digit-argument-fn 9))
  (define-key evil-motion-state-map ")" (capslock-digit-argument-fn 0))

  (define-key evil-motion-state-map (kbd "0") 'evil-beginning-of-line)
  (define-key evil-normal-state-map "2" 'evil-record-macro)
  (define-key evil-motion-state-map (kbd "3") 'evil-scroll-down)
  (define-key evil-motion-state-map (kbd "4") 'evil-scroll-up)
  (define-key evil-motion-state-map (kbd "5") 'evil-first-non-blank)
  (define-key evil-motion-state-map (kbd "8") 'evil-end-of-line)
  ;;; The original bindings felt unnatural, swapped them
  (define-key evil-motion-state-map "{" 'evil-forward-paragraph)
  (define-key evil-motion-state-map "}" 'evil-backward-paragraph)

  (define-key evil-normal-state-map "a"           'evil-append)

  (define-key evil-motion-state-map "h"           'evil-backward-char)
  (define-key evil-motion-state-map "j"           'evil-next-visual-line)
  (define-key evil-motion-state-map "k"           'evil-previous-visual-line)
  (define-key evil-motion-state-map "l"           'evil-forward-char)
  (define-key evil-motion-state-map (kbd "M-u")   'evil-scroll-down)
  (define-key evil-motion-state-map (kbd "M-i")   'evil-scroll-up)

  (define-key isearch-mode-map (kbd "<up>")   'isearch-ring-retreat)
  (define-key isearch-mode-map (kbd "<down>") 'isearch-ring-advance)
  (define-key isearch-mode-map (kbd "M-k")    'isearch-ring-retreat)
  (define-key isearch-mode-map (kbd "M-j")    'isearch-ring-advance)
  (define-key evil-ex-search-keymap (kbd "M-k") 'previous-history-element)
  (define-key evil-ex-search-keymap (kbd "M-j") 'next-history-element)
  (define-key evil-ex-search-keymap (kbd "C-k") 'previous-history-element)
  (define-key evil-ex-search-keymap (kbd "C-j") 'next-history-element)
  (define-key evil-ex-search-keymap (kbd "C-w") 'backward-kill-word)

  ;;; https://github.com/bling/evil-visualstar
  (defun evil-visualstar-begin-search (beg end direction)
    (evil-exit-visual-state)
    (let ((selection (regexp-quote (buffer-substring-no-properties beg end))))
      (let ((pattern (evil-ex-make-search-pattern selection))
            (direction (if direction 'forward 'backward)))
        (setq evil-ex-search-direction direction)
        (setq evil-ex-search-pattern pattern)
        (evil-ex-search-activate-highlight pattern)
        ;; update search history unless this pattern equals the
        ;; previous pattern
        (unless (equal (car-safe evil-ex-search-history) selection)
          (push selection evil-ex-search-history))
        (evil-push-search-history selection (eq direction 'forward))
        (evil-ex-search-next)
        )
      ))
  (evil-define-motion my-evil-visualstar-search (beg end)
    "Search for the visual selection forwards."
    :repeat nil
    (interactive "<r>")
    (evil-visualstar-begin-search beg end t)
    (goto-char beg)
    )
  (evil-define-motion my-evil-ex-search-word (count &optional symbol)
    "Search for the next occurrence of word under the cursor."
    :type exclusive
    (interactive (list (prefix-numeric-value current-prefix-arg)
                       evil-symbol-word-search))
    (let ((pos (point)))
      (evil-ex-start-word-search nil 'forward count symbol)
      (goto-char pos)
      )
    )
  (evil-define-key 'visual my-intercept-mode-map (kbd "C-n") #'my-evil-visualstar-search)
  (evil-define-key 'normal my-intercept-mode-map (kbd "C-n") #'my-evil-ex-search-word)

  (evil-define-motion my-evil-ex-search-backward (count)
    "Starts a forward search."
    :jump t
    :repeat evil-repeat-ex-search
    (evil-ex-start-search 'backward count)
    (setq evil-ex-search-direction 'forward)
    )
  (define-key evil-motion-state-map "?" 'my-evil-ex-search-backward)

  (define-key evil-motion-state-map (kbd "C-s") 'evil-write)

  (define-key evil-motion-state-map (kbd "M-h") 'evil-jump-backward)
  (define-key evil-motion-state-map (kbd "M-l") 'evil-jump-forward)

  (define-key evil-motion-state-map (kbd "M-c") 'evil-visual-block)

  (define-key evil-normal-state-map "U" 'redo)
  (define-key evil-normal-state-map "Y" 'evil-join)
  (define-key evil-motion-state-map "Y" 'evil-join)

  (evil-ex-define-cmd "Q"  'evil-quit)
  (evil-ex-define-cmd "Qa" 'evil-quit-all)
  (evil-ex-define-cmd "QA" 'evil-quit-all)

  ;; ex command line
  (define-key evil-ex-completion-map "\d" #'evil-ex-delete-backward-char)
  (define-key evil-ex-completion-map "\t" #'evil-ex-completion)
  (define-key evil-ex-completion-map [tab] #'evil-ex-completion)
  (define-key evil-ex-completion-map [remap completion-at-point] #'evil-ex-completion)
  (define-key evil-ex-completion-map "\C-a" 'move-beginning-of-line)
  (define-key evil-ex-completion-map "\C-e" 'move-end-of-line)
  (define-key evil-ex-completion-map "\C-w" 'backward-kill-word)
  (define-key evil-ex-completion-map "\C-c" 'abort-recursive-edit)
  (define-key evil-ex-completion-map "\C-d" 'kill-line)
  (define-key evil-ex-completion-map "\C-g" 'abort-recursive-edit)
  (define-key evil-ex-completion-map "\C-k" 'evil-insert-digraph)
  (define-key evil-ex-completion-map "\C-l" 'evil-ex-completion)
  (define-key evil-ex-completion-map "\C-p" #'previous-complete-history-element)
  (define-key evil-ex-completion-map "\C-r" 'evil-paste-from-register)
  (define-key evil-ex-completion-map "\C-n" #'next-complete-history-element)
  (define-key evil-ex-completion-map "\C-u" 'evil-delete-whole-line)
  (define-key evil-ex-completion-map "\C-v" #'quoted-insert)
  (define-key evil-ex-completion-map [escape] 'abort-recursive-edit)
  (define-key evil-ex-completion-map [S-left] 'backward-word)
  (define-key evil-ex-completion-map [S-right] 'forward-word)
  (define-key evil-ex-completion-map "\M-k" [up])
  (define-key evil-ex-completion-map "\M-j" [down])
  (define-key evil-ex-completion-map "\C-k" [up])
  (define-key evil-ex-completion-map "\C-j" [down])
  (define-key evil-ex-completion-map "\M-h" [left])
  (define-key evil-ex-completion-map "\M-l" [right])
  (define-key evil-ex-completion-map "\C-h" [left])
  (define-key evil-ex-completion-map "\C-l" [right])
  (define-key evil-ex-completion-map [prior] 'previous-history-element)
  (define-key evil-ex-completion-map [next] 'next-history-element)
  (define-key evil-ex-completion-map [return] 'exit-minibuffer)
  (define-key evil-ex-completion-map (kbd "RET") 'exit-minibuffer)

  (define-key evil-ex-search-keymap "\M-k" [up])
  (define-key evil-ex-search-keymap "\M-j" [down])
  (define-key evil-ex-search-keymap "\C-k" [up])
  (define-key evil-ex-search-keymap "\C-j" [down])
  (define-key evil-ex-search-keymap "\M-h" [left])
  (define-key evil-ex-search-keymap "\M-l" [right])
  (define-key evil-ex-search-keymap "\C-h" [left])
  (define-key evil-ex-search-keymap "\C-l" [right])
  )

(with-eval-after-load 'evil-easymotion
  (define-key evil-normal-state-map "J" nil)

  (evil-define-key 'motion my-intercept-mode-map "t" #'evilem-motion-find-char-backward)
  (evil-define-key 'motion my-intercept-mode-map "T" #'evilem-motion-find-char-to-backward)
  (evil-define-key 'motion my-intercept-mode-map "f" #'evilem-motion-find-char)
  (evil-define-key 'motion my-intercept-mode-map "F" #'evilem-motion-find-char-to)

  (evil-define-motion my-evilem-backward-word-begin(count)
    (let ((last (point)))
      (call-interactively #'evil-backward-word-begin)
      (while (and (not (= (char-syntax (char-after (point))) ?w))
                  (not (= last (point)))
               )
        (setq last (point))
        (call-interactively #'evil-backward-word-begin))
    ))
  (evil-define-motion my-evilem-backward-word-end(count)
    (let ((last (point)))
      (call-interactively #'evil-backward-word-end)
      (while (and (not (= (char-syntax (char-after (point))) ?w))
                  (not (= last (point)))
               )
        (setq last (point))
        (call-interactively #'evil-backward-word-end))
    ))
  (evil-define-motion my-evilem-forward-word-begin(count)
    (let ((last (point)))
      (call-interactively #'evil-forward-word-begin)
      (while (and (not (= (char-syntax (char-after (point))) ?w))
                  (not (= last (point)))
               )
        (setq last (point))
        (call-interactively #'evil-forward-word-begin))
    ))
  (evil-define-motion my-evilem-forward-word-end(count)
    (let ((last (point)))
      (call-interactively #'evil-forward-word-end)
      (while (and (not (= (char-syntax (char-after (point))) ?w))
                  (not (= last (point)))
               )
        (setq last (point))
        (call-interactively #'evil-forward-word-end))
    ))
  (evilem-define "H" #'my-evilem-backward-word-begin)
  (evilem-define "J" #'my-evilem-backward-word-end)
  (evilem-define "K" #'my-evilem-forward-word-begin)
  (evilem-define "L" #'my-evilem-forward-word-end)
  )

(with-eval-after-load 'counsel-projectile
  (defun my-counsel-projectile--project-buffers-and-files (&rest _)
    ;; The ignored arguments are so that the function can be used as
    ;; collection function in `counsel-projectile'.
    "Return a list of buffers and non-visited files in the current
    project.  Buffers and files are separately sorted depending on
    `counsel-projectile-sort-buffers' and
    `counsel-projectile-sort-files', respectively."
    (let ((buffers (ivy--buffer-list "" nil))
          (files (projectile-current-project-files))
          (root (projectile-project-root))
          file sort-fn)
      ;; Sort buffers and files depending on
      ;; `counsel-projectile-sort-buffers' and
      ;; `counsel-projectile-sort-files', respectively.
      ;; We need to do this here because matching will be done against
      ;; the variables `counsel-projectile--buffers' and
      ;; `counsel-projectile--non-visited-files', not against the
      ;; returned collection, so ivy's native sorting mechanism won't
      ;; work.
      (when (and counsel-projectile-sort-buffers
                 (<= (length buffers) ivy-sort-max-size)
                 (setq sort-fn (ivy--sort-function 'counsel-projectile-switch-to-buffer)))
        (setq buffers (sort (copy-sequence buffers) sort-fn)))
      (when (and counsel-projectile-sort-files
                 (<= (length files) ivy-sort-max-size)
                 (setq sort-fn (ivy--sort-function 'counsel-projectile-find-file)))
        (setq files (sort (copy-sequence files) sort-fn)))
      ;; Finally, bind `counsel-projectile--buffers' and
      ;; `counsel-projectile--non-visited-files' and return the whole
      ;; collection.
      (append (setq counsel-projectile--buffers buffers)
              (setq counsel-projectile--non-visited-files files))))
  (defun my-counsel-projectile (&optional arg)
    "Jump to a buffer or file in the current project.
  With a prefix ARG, invalidate the cache first.
  If not inside a project, call `counsel-projectile-switch-project'."
    (interactive "P")
    (if (not (projectile-project-p))
        (ivy-switch-buffer)
      (projectile-maybe-invalidate-cache arg)
      (ivy-read (projectile-prepend-project-name "DST: ")
                ;; We use a collection function so that it is called each
                ;; time the `ivy-state' is reset. This is needed for the
                ;; "kill buffer" action.
                #'my-counsel-projectile--project-buffers-and-files
                :matcher #'counsel-projectile--matcher
                :require-match t
                :action counsel-projectile-action
                :keymap counsel-projectile-map
                :caller 'counsel-projectile)))

  (my-global-define-key (kbd "M-w") 'my-counsel-projectile)
  ;;; find-file sucks - only searchs in cwd
  ;;; find-jump sucks - bad performance
  (my-global-define-key (kbd "M-e") 'counsel-fzf)
  (my-global-define-key (kbd "M-a") 'counsel-projectile-rg)
  (my-global-define-key (kbd "M-s") 'counsel-projectile-git-grep)
  (my-global-define-key (kbd "M-d") (lambda ()
    (interactive)
    (if (not (projectile-project-p))
        (counsel-find-file)
        (counsel-git)
        )
    ))

  (my-global-define-key (kbd "M-x") 'counsel-M-x)
  (evil-define-key 'normal my-intercept-mode-map (kbd "DEL") 'swiper)
  (evil-define-key 'normal my-intercept-mode-map (kbd "C-DEL") 'swiper-all)
  (evil-define-key 'normal my-intercept-mode-map [backspace] 'swiper)
  (evil-define-key 'normal my-intercept-mode-map [(control backspace)] 'swiper-all)

  (evil-define-key 'normal my-intercept-mode-map (kbd "SPC i r") 'ivy-resume)
  (evil-define-key 'normal my-intercept-mode-map (kbd "SPC u g") 'magit-status)
  (evil-define-key 'normal my-intercept-mode-map (kbd "SPC u h") 'evil-ex-nohighlight)
  ;;; it is actually find file command
  (evil-define-key 'normal my-intercept-mode-map (kbd "SPC u r") 'counsel-rg)
  (evil-define-key 'normal my-intercept-mode-map (kbd "SPC u i") 'counsel-git)
  (evil-define-key 'normal my-intercept-mode-map (kbd "SPC u s") 'my-shell)
  (evil-define-key 'normal my-intercept-mode-map (kbd "SPC u ?") 'counsel-apropos)
  )

(use-package expand-region
  :config
    ;; (define-key evil-motion-state-map (kbd "7") 'er/expand-symbol)
  )

(use-package evil-nerd-commenter
  :config
  (define-key evil-normal-state-map (kbd "gcc") 'evilnc-comment-or-uncomment-lines)
  (define-key evil-visual-state-map (kbd "gc") 'comment-or-uncomment-region)
  )

(use-package evil-surround
  :config
  ;;; Add surrounding
  ;;; You can surround in visual-state with S<textobject> or gS<textobject>. Or in
  ;;; normal-state with ys<textobject> or yS<textobject>.
  ;;; Change surrounding
  ;;; You can change a surrounding with cs<old-textobject><new-textobject>.
  ;;; Delete surrounding
  ;;; Y ou can delete a surrounding with ds<textobject>.
  (global-evil-surround-mode t)
  )

;;; Match everything
;;; https://github.com/redguardtoo/evil-matchit
(use-package evil-matchit
  :config
  (defun evilmi-customize-keybinding()
    (evil-define-key 'normal evil-matchit-mode-map "9" 'evilmi-jump-items)
    (evil-define-key 'visual evil-matchit-mode-map "9" 'evilmi-jump-items)
    )
  (global-evil-matchit-mode 1)
  )

(provide 'evil-config)
;;; evil-config.el ends here
