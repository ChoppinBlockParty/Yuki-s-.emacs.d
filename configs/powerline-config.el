;;; powerline-config --- Power mode line
;;; Commentary:
;;; Code:
(use-package which-func
  :ensure nil
  :config
  (defconst my-which-func-current
    '(:eval
      (let ((s (or (gethash (selected-window) which-func-table) which-func-unknown)))
        (replace-regexp-in-string "%" "%%" (substring s 0 (min (length s) 30)))
        )
      )
    )
  (setq
     which-func-unknown "☢"
     which-func-format '((:propertize my-which-func-current))
    )
  (which-function-mode 1)
  )

(use-package powerline
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

  (setq powerline-default-separator 'contour
        powerline-default-separator-dir '(left . right)
        powerline-display-buffer-size nil
        powerline-display-hud nil
        powerline-display-mule-info nil
        powerline-gui-use-vcs-glyph t
        )

  (setq eldoc-minor-mode-string nil)

  (defun my-mode-line-modified ()
    "An `all-the-icons' segment depicting the current buffers state"
    ;; buffer-state
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
                          )
                  'local-map (make-mode-line-mouse-map 'mouse-1 'read-only-mode)
                  )
      )
    )

  (defun my-mode-line-filepath ()
  "Displays *my* version of displaying the evil state."
    (let ((val buffer-file-truename))
      (if val val "")
      ))

  (defun my-mode-line-mode-icon ()
    "An `all-the-icons' segment indicating the current buffer's mode with an icon"
    ;; (format "%s" major-mode)
    ;;; Unfortunately produces rendering glitches, ...
    (let ((icon (all-the-icons-icon-for-mode major-mode
                  )))
        (unless (symbolp icon)
          (propertize icon
                      'help-echo (format "Major-mode: `%s'" major-mode)
                      'display '(raise 0.0)
                      'face `(:family ,(all-the-icons-icon-family-for-mode major-mode)
                              :inherit)
                      ))
        )
    )

  (defun my-mode-line-window-number ()
    "Return the number of the window."
    (let* ((window-number (window-numbering-get-number))
           (str (cond
              ((not window-number)  "")
              ((= window-number 1)  "1")
              ((= window-number 2)  "2")
              ((= window-number 3)  "3")
              ((= window-number 4)  "4")
              ((= window-number 5)  "5")
              ((= window-number 6)  "6")
              ((= window-number 7)  "7")
              ((= window-number 8)  "8")
              ((= window-number 9)  "8")
              ((= window-number 0)  "0")
              )))
          (propertize str 'face '(:inherit))
          )
    )

  (defun my-modeline-flycheck-status ()
    (let* ((text (pcase flycheck-last-status-change
                  (`finished (if flycheck-current-errors
                                 (let ((count (let-alist (flycheck-count-errors flycheck-current-errors)
                                                (+ (or .warning 0) (or .error 0)))))
                                   (format "⚡ %s" count))
                                "✔"))
                  (`running     "⟲")
                  (`no-checker  "☣")
                  (`not-checked "⚇")
                  (`errored     "⛈")
                  (`interrupted "⛔")
                  (`suspicious  "☠"))))
       (propertize text
                   'help-echo "Show Flycheck Errors"
                   'face '(:inherit)
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
           (lhs (list (powerline-raw (my-mode-line-window-number) face0)
                      (powerline-raw (my-mode-line-modified) face0)
                      (powerline-buffer-id face0)
                      (powerline-raw "%4l" face0)
                      (powerline-raw ":" face0)
                      (powerline-raw "%3c" face0)
                      (powerline-raw "%p" face0 'l)
                      (funcall separator-left face0 face1)
                      (powerline-raw which-func-format face1)
                      (powerline-narrow face0 'l)
                      (funcall separator-right face1 face2)
                      (powerline-raw (my-mode-line-mode-icon) face2)
                      (funcall separator-left face2 face1)
                      (powerline-raw (my-modeline-flycheck-status) face1)
                      (funcall separator-right face1 face2)
                      (powerline-minor-modes face2)
                      (funcall separator-left face2 face1)
                      (powerline-raw (my-mode-line-filepath) face1)
                      )))
           (concat (powerline-render lhs)
                   (powerline-fill face1 0)
                   ))))))

  (powerline-my-theme)
  )

(provide 'powerline-config)
;;; powerline-config.el ends here
