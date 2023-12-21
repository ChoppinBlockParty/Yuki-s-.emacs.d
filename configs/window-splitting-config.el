;;; window-splitting --- My window splitting logic
;;; Commentary:
;;;
;;; Changes how windows are spawned
;;;
;;; See help fro (display-buffer) to understand what is going on.
;;; Code:


(defvar my-last-preferred-splits '())

(defun my-reuse-last-preferred-split-save (win new-win)
  "Save WIN NEW-WIN for reuse."
  (when new-win
    (add-to-list 'my-last-preferred-splits `(,win . ,new-win))
    )
  new-win)

(defun my-reuse-last-preferred-split (window)
  "Reuse split for WINDOW."
  (let ((new-list '()))
    (dolist (val my-last-preferred-splits)
      (when (and (window-live-p (car val)) (window-live-p (cdr val)))
        (add-to-list 'new-list val)
        )
      )
    (setq my-last-preferred-splits new-list)
  )
  (let ((old-win (cdr (assq window my-last-preferred-splits))))
    old-win)
  )

(setq split-window-preferred-function (lambda (&optional window)
  "Split WINDOW."
  (let
    ((window (or window (selected-window))))
    (or
      (my-reuse-last-preferred-split window)
      (and (window-splittable-p window)
           (my-reuse-last-preferred-split-save
             window
             (with-selected-window window (split-window-below))
             )
           )
      (and (window-splittable-p window t)
           (my-reuse-last-preferred-split-save
             window
             (with-selected-window window (split-window-right))
             )
           )
      (and
        ;; If WINDOW is the only usable window on its frame (it is
        ;; the only one or, not being the only one, all the other
        ;; ones are dedicated) and is not the minibuffer window, try
        ;; to split it vertically disregarding the value of
        ;; `split-height-threshold'.
        (let ((frame (window-frame window)))
          (or
           (eq window (frame-root-window frame))
           (catch 'done
             (walk-window-tree (lambda (w)
                                 (unless (or (eq w window)
                                             (window-dedicated-p w))
                                   (throw 'done nil)))
                               frame)
             t)))
         (not (window-minibuffer-p window))
         (let ((split-height-threshold 0))
              (when (window-splittable-p window)
                    (my-reuse-last-preferred-split-save
                      window
                      (with-selected-window window (split-window-below))
                      )
                    )
              )
         )
      (and
         (let ((split-height-threshold 0))
              (when (window-splittable-p window)
                    (my-reuse-last-preferred-split-save
                      window
                      (with-selected-window window (split-window-below))
                      )
                    )
              )
         )
    ))
  ))

(defun my-display-buffer-find-major-mode-window (mode &optional mode-two)
  "Find window. MODE to look up. MODE-TWO optional."
  (let (cur-win ret-win (l (window-list)))
    (while (and l (not ret-win))
           (setq cur-win (car l)
                 l       (cdr l))
           (with-current-buffer (window-buffer cur-win)
             (cond ((equal major-mode mode)     (setq ret-win cur-win))
                   ((equal major-mode mode-two) (setq ret-win cur-win))
                   ))
           )
    ret-win))

(defun my-window-display-buffer-create-split (window)
  "Create split relative to WINDOW."
  (let
    (new-win)
    (setq new-win (window--try-to-split-window window))
    (unless new-win
      (setq new-win (window--try-to-split-window (get-lru-window t t))))
    (unless new-win
      (setq new-win window))
    (unless new-win
      (setq new-win (selected-window)))
    new-win))

(defun my-window-display-buffer (buffer window)
  "Display. BUFFER to display. WINDOW to show into."
  (if (or (window-minibuffer-p) (window-dedicated-p))
      (window--display-buffer
        buffer
        (my-window-display-buffer-create-split window)
        'reuse '((inhibit-switch-frame . t)))
      (window--display-buffer
        buffer
        window
        'reuse '((inhibit-switch-frame . t)))
      ))

(defun my-window-display-buffer-split (buffer)
  "Split. BUFFER to display relative to WIN or `selected-window`."
  (my-window-display-buffer buffer
                            (my-window-display-buffer-create-split (selected-window))
                            ))

(defun my-window-display-buffer-match-any (str &rest matchers)
  "Match STR to any of MATCHERS."
  (let (match-p m res)
    (while (and (not match-p) matchers)
      (setq m (car matchers)
            matchers (cdr matchers)
            res (string-match-p m str)
            )
      (when (and res (= res 0)) (setq match-p t))
      )
    match-p)
  )

(defun my-display-buffer-action (buffer alist)
  "Display BUFFER in the selected window with ALIST."
  (let
    ((sel-buf (window-buffer (selected-window))) sel-mode new-mode)
    (with-current-buffer sel-buf
      (setq sel-mode major-mode))
    (with-current-buffer buffer
      (setq new-mode major-mode))
    ;; (print sel-mode)
    ;; (print (buffer-name sel-buf))
    ;; (print new-mode)
    ;; (print (buffer-name buffer))
    ;; (print alist)
    (cond
      ;;; 1. Reuse window, if exists, for help buffers.
      ((or (equal new-mode 'apropos-mode) (equal new-mode 'help-mode))
       (let ((win (my-display-buffer-find-major-mode-window 'apropos-mode 'help-mode)))
         (if win
             (my-window-display-buffer buffer win)
             (my-window-display-buffer-split buffer)
             ))
       )
      ;;; 2. For magit pop-ups that are called transient with a space in front
      ;;; For regex syntax see this - https://www.gnu.org/software/emacs/manual/html_node/elisp/Regexp-Backslash.html.
      ((and
         (equal new-mode 'fundamental-mode)
         (my-window-display-buffer-match-any (buffer-name buffer) "\\`\s-*\\*transient\\*\\'")
         )
       ;;; transient windows are evil, let the default display function to handle it,
       ;;; though I don't like the default behavior.
       nil
       ;; (let ((new-win (with-selected-window (selected-window) (split-window-below))))
       ;;   (set-window-dedicated-p new-win 1)
       ;;   (my-window-display-buffer buffer new-win))
       )
      ;;; 3. Let the following modes to be placed as they request.
      ((or
         (member new-mode '(dired-sidebar-mode
                            magit-popup-mode
                            rg-mode
                            ivy-occur-grep-mode
                            completion-list-mode
                            compilation-mode))
         (my-window-display-buffer-match-any (buffer-name buffer) "\\`\\*Completions\\*\\'")
         )
       nil
       )
      ;;; 4. Always split for commit message and for magit log.
      ((or
         (and (equal sel-mode 'text-mode)
              (my-window-display-buffer-match-any (buffer-name sel-buf) "\\`COMMIT_EDITMSG"))
         (equal sel-mode 'magit-log-mode)
         )
       (my-window-display-buffer-split buffer)
       )
      ;;; I do not use dired-sidebar anymore.
      ;; ((equal sel-mode 'dired-sidebar-mode)
      ;;  (my-window-display-buffer buffer (get-mru-window nil nil t))
      ;;  )
      ;;; 5. Everything else split
      (t
       (if (cdr (assq 'inhibit-same-window alist))
           (my-window-display-buffer-split buffer)
           (my-window-display-buffer buffer (selected-window))
           )
       )
      )
    ))

(add-to-list 'display-buffer-alist '(".*" (my-display-buffer-action)))

(provide 'window-splitting-config)
;;; window-splitting-config.el ends here
