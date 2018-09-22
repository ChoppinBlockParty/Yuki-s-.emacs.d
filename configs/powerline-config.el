;;;

;;; Code
(use-package powerline
  :after (all-the-icons)
  :init
 (defface powerline-active1 '((t (:foreground "#d0d0f0" :background "purple" :inherit mode-line)))
   "Powerline face 1."
   :group 'powerline)
 (defface powerline-active2 '((t (:foreground "#63b132" :background "black" :inherit mode-line)))
   "Powerline face 2."
   :group 'powerline)
 (defface powerline-active0 '((t (:foreground "purple" :background "#d0d0f0" :inherit mode-line)))
   "Powerline face 0."
   :group 'powerline)
 (defface powerline-inactive0
   '((t (:background "black" :inherit mode-line-inactive)))
   "Powerline face 0."
   :group 'powerline)
  :config
  ;;; Nyan is dumbly amazing, but... CPU cycles, omg...
  ;; (use-package nyan-mode
  ;;   :ensure t
  ;;   :config
  ;;   (setq
  ;;     ;;; t to have it animated, nil for a static version.
  ;;     nyan-animate-nyancat t
  ;;     ;;; number of seconds between animation frames. Accepts fractional values.
  ;;     nyan-animation-frame-interval 0.5
  ;;     ;;; length of nyan-mode bar, in 8px-wide units.
  ;;     nyan-bar-length 32
  ;;     ;;; choose a cat face for the console mode.
  ;;     ;; nyan-cat-face-number
  ;;     ;;; to make the trail wavy; works even better when animation is enabled!
  ;;     ;; nyan-wavy-trail t
  ;;     ;;; minimum width of the window, below which Nyan Mode will be disabled. This is important because Nyan Mode will otherwise push out more relevant information from the mode-line.
  ;;     ;; nyan-minimum-window-width
  ;;     )
  ;;   (nyan-start-animation)
  ;;   )

  ;;; (defcustom powerline-default-separator 'wave
  ;;; "The separator to use for the default theme."
  ;;; :group 'powerline
  ;;; :type '(choice (const alternate)
  ;;;                 (const arrow)
  ;;;                 (const arrow-fade)
  ;;;                 (const bar)
  ;;;                 (const box)
  ;;;                 (const brace)
  ;;;                 (const butt)
  ;;;                 (const chamfer)
  ;;;                 (const contour)
  ;;;                 (const curve)
  ;;;                 (const rounded)
  ;;;                 (const roundstub)
  ;;;                 (const slant)
  ;;;                 (const wave)
  ;;;                 (const zigzag)
  ;;;                 (const nil)))
  (setq powerline-default-separator 'contour
        powerline-default-separator-dir '(left . right)
        ;; powerline-height 10
        powerline-display-buffer-size nil
        powerline-display-hud nil
        powerline-display-mule-info nil
        powerline-gui-use-vcs-glyph t
        )

  (setq eldoc-minor-mode-string nil)
  (setq which-func-unknown "☢")
  (which-function-mode 1)

  (defun custom-modeline-modified ()
    "An `all-the-icons' segment depicting the current buffers state"
    (let* ((buffer-state (format-mode-line "%*"))
           (icon (cond
                  ((string= buffer-state "-") "link")
                  ((string= buffer-state "*") "chain-broken")
                  ((string= buffer-state "%") "lock")
                  )
                 )
           )
      (propertize (all-the-icons-faicon icon :v-adjust 0.0)
                  'face `(:family ,(all-the-icons-faicon-family)
                          :inherit)
                  'local-map (make-mode-line-mouse-map 'mouse-1 'read-only-mode)
                  )
      )
    )

  (defun my-mode-line-evil-state ()
  "Displays *my* version of displaying the evil state."
    (case evil-state
      ('normal "Ⓝ")
      ('insert "Ⓘ")
      ('visual "Ⓥ")
      ('motion "Ⓜ")
      (t       "Ⓔ")))

  (defun custom-modeline-mode-icon ()
    "An `all-the-icons' segment indicating the current buffer's mode with an icon"
    (let ((icon (all-the-icons-icon-for-mode major-mode)))
        (unless (symbolp icon)
          (propertize icon
                      'help-echo (format "Major-mode: `%s'" major-mode)
                      'display '(raise 0)
                      'face `(:family ,(all-the-icons-icon-family-for-mode major-mode)
                              :inherit)
                      )
          )
        )
     )

  (defun custom-modeline-flycheck-status (face)
    (let* ((text (pcase flycheck-last-status-change
                  (`finished (if flycheck-current-errors
                                 (let ((count (let-alist (flycheck-count-errors flycheck-current-errors)
                                                (+ (or .warning 0) (or .error 0)))))
                                   (format "⚡ %s" count ))
                                "✔"))
                  (`running     "⟲")
                  (`no-checker  "☣")
                  (`not-checked "⚇")
                  (`errored     "⛈")
                  (`interrupted "⛔")
                  (`suspicious  "☠"))))
       (propertize text
                   'help-echo "Show Flycheck Errors"
                   'face face
                   'mouse-face '(:box 1)
                   'local-map (make-mode-line-mouse-map
                               'mouse-1 (lambda () (interactive) (flycheck-list-errors))))
       )
    )

  (defun powerline-my-theme ()
    "Customisation of the default powerline theme"
    (interactive)
    (setq-default mode-line-format
      '("%e"
        (:eval
         (let* (
           (active (powerline-selected-window-active))
           (mode-line (if active 'mode-line 'mode-line-inactive))
           (face0 (if active 'powerline-active0 'powerline-inactive0))
           (face1 (if active 'powerline-active1 'powerline-inactive1))
           (face2 (if active 'powerline-active2 'powerline-inactive2))
           (black-face 'powerline-inactive0)
           (separator-left
            (intern
             (format "powerline-%s-%s"
                     powerline-default-separator
                     (car powerline-default-separator-dir))))
           (separator-right
            (intern (format "powerline-%s-%s"
                            powerline-default-separator
                            (cdr powerline-default-separator-dir))))
           (lhs (list ;; (powerline-raw "%*" face0 'l)
                      (powerline-raw (list (custom-modeline-modified)) face0)
                      ;; (powerline-raw (list (my-mode-line-evil-state)) face0)
                      (powerline-buffer-id face0 'l)
                      (powerline-raw "%4l" face0 'r)
                      (powerline-raw ":" face0)
                      (powerline-raw "%3c" face0 'r)
                      (powerline-raw "%6p" face0 'r)
                      (funcall separator-left face0 face1)
                      ;; (powerline-hud face2 face1)
                      ;; (powerline-raw (list (nyan-create)) black-face 'l)
                      (powerline-raw which-func-format face1 'l)
                      (powerline-narrow face0 'l)
                      (funcall separator-right face1 face2)
                      ;; (powerline-major-mode face1 'l)
                      (powerline-raw (list (custom-modeline-mode-icon)) face2)
                      (funcall separator-left face2 face1)
                      (custom-modeline-flycheck-status face1)
                      (funcall separator-right face1 face2)
                      (powerline-minor-modes face2 'l)
                      ;; (powerline-narrow face1)
                      (funcall separator-left face2 face1)
                      ;; (powerline-vc face1 'r)
                      ))
           (rhs (list
                 ;; (powerline-raw global-mode-string face2 'r)
                      )))
           (concat (powerline-render lhs)
                   (powerline-fill face1 (powerline-width rhs))
                   (powerline-render rhs)))))))


  ;; (powerline-moe-theme)
  ;; (powerline-vim-theme)
  (powerline-my-theme)
  )

(provide 'powerline-config)
