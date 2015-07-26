(add-to-list 'load-path (concat user-emacs-directory "configs"))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("SC"  . "http://joseito.republika.pl/sunrise-commander/")))

(require 'package)
(package-initialize)
(setq package-enable-at-startup nil)
(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

(require 'use-package)

(require 'configs-base)
(require 'config-color)
(require 'config-cl-lib)
(require 'configs-evil)
(require 'configs-dired)
(require 'configs-helm)
(require 'configs-projectile)
; (require 'config-sunrise-commander)
(require 'config-ranger)
(require 'config-paren)
(require 'config-rainbow-delimiters)
(require 'config-autocompletion)
(require 'config-cider)

; (cua-mode t)
; (setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
;     (transient-mark-mode t) ;; No region when it is not highlighted
;     (setq cua-keep-region-after-copy t) ;; Standard Windows behaviour

; (cua-selection-mode t)
; (setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
; (transient-mark-mode 1) ;; No region when it is not highlighted
; (setq cua-keep-region-after-copy t) ;; Standard Windows behaviour
; (setq x-select-enable-clipboard t)
; (setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
;     (transient-mark-mode 1) ;; No region when it is not highlighted
;     (setq cua-keep-region-after-copy t) ;; Standard Windows behaviour

; (add-hook 'evil-mode-hook 'evil-mode-bindings)

; (defun evil-mode-bindings ()
;   "Bind symbols to digits."
;   (define-key key-translation-map (kbd "!") "1")
;   (define-key key-translation-map (kbd "@") "2")
;   (define-key key-translation-map (kbd "#") "3")
;   (define-key key-translation-map (kbd "$") "4")
;   (define-key key-translation-map (kbd "%") "5")
;   (define-key key-translation-map (kbd "^") "6")
;   (define-key key-translation-map (kbd "&") "7")
;   (define-key key-translation-map (kbd "*") "8")
;   (define-key key-translation-map (kbd "(") "9")
;   (define-key key-translation-map (kbd ")") "0"))

; (setq evil-leader/in-all-states 1)

; (defun my-digit-argument-5 ()
;   (interactive)
;   (digit-argument 5))

; (define-key evil-normal-state-map "5" 'evil-beginning-of-line)
; (define-key evil-normal-state-map "%" 'my-digit-argument-5)
; (define-key evil-normal-state-map "8" 'evil-end-of-line)
; (define-key evil-normal-state-map "*" 'digit-argument)

; (define-key evil-normal-state-map "\M-q" (lambda ()
;     (interactive)
;     (dired "~/")))
; (define-key evil-normal-state-map "\M-a" (lambda ()
;     (interactive)
;     (dired "~/")))
; (evil-leader/set-key "e" 'pp-eval-last-sexp)
; (evil-leader/set-key "," 'other-window)
; (evil-leader/set-key "b" 'ibuffer)
; (evil-leader/set-key "x" 'helm-M-x)

; (require 'helm)
; (require 'helm-config)
; (defhydra helm-like-unite (:hint nil
;                            :color pink)
;   ; "
; ; Nav ^^^^^^^^^        Mark ^^          Other ^^       Quit
; ; ^^^^^^^^^^------------^^----------------^^----------------------
; ; _K_ ^ ^ _k_ ^ ^     _m_ark           _v_iew         _i_: cancel
; ; ^↕^ _h_ ^✜^ _l_     _t_oggle mark    _H_elp         _o_: quit
; ; _J_ ^ ^ _j_ ^ ^     _U_nmark all     _d_elete
; ; ^^^^^^^^^^                           _f_ollow: %(helm-attr 'follow)
; ; "
;   ;; arrows
;   ("h" helm-beginning-of-buffer)
;   ("j" helm-next-line)
;   ("k" helm-previous-line)
;   ("l" helm-end-of-buffer)
;   ;; beginning/end
;   ("g" helm-beginning-of-buffer)
;   ("G" helm-end-of-buffer)
;   ;; scroll
;   ("K" helm-scroll-other-window-down)
;   ("J" helm-scroll-other-window)
;   ;; mark
;   ("v" helm-toggle-visible-mark)
;   ("t" helm-toggle-all-marks)
;   ("u" helm-unmark-all)
;   ;; exit
;   ("<escape>" keyboard-escape-quit "" :exit t)
;   ("o" keyboard-escape-quit :exit t)
;   ("i" nil)
;   ;; sources
;   ("}" helm-next-source)
;   ("{" helm-previous-source)
;   ;; rest
;   ("H" helm-help)
;   ("p" helm-execute-persistent-action)
;   ("d" helm-persistent-delete-marked)
;   ("f" helm-follow-mode))


; (defun helm-persistent-delete-marked ()
;   "Kill buffer without quitting helm."
;   (interactive)
;   (if (equal (cdr (assoc 'name (helm-get-current-source)))
;              "Buffers")
;       (with-helm-alive-p
;         (helm-attrset 'kill-action
;                       '(helm-persistent-kill-buffers . never-split))
;         (helm-execute-persistent-action 'kill-action))
;     (user-error "Only works for buffers")))
; (defun helm-persistent-kill-buffers (_buffer)
;   (unwind-protect
;        (dolist (b (helm-marked-candidates))
;          (helm-buffers-persistent-kill-1 b))
;     (with-helm-buffer
;       (setq helm-marked-candidates nil
;             helm-visible-mark-overlays nil))
;     (helm-force-update (helm-buffers--quote-truncated-buffer
;                         (helm-get-selection)))))

; (evil-leader/set-key
;         "f" 'helm-for-files
;         ; "l" 'helm-locate
;         "c" 'helm-colors
;         "y" 'helm-show-kill-ring
;         "t" 'helm-top
;         "m" 'helm-man-woman
;         "o" 'helm-occur
;         ; "j" 'helm-M-x
;         "e" 'helm-find-files
;         "b" 'helm-buffers-list
;         ; "h" 'helm-projectile-find-file
;         "r" 'helm-recentf
;         ; "H" 'helm-projectile
;         )

; (require 'auto-complete-config)
; (require 'flx-ido)
; (flx-ido-mode 1)
; (setq flx-ido-threshhold 300)
; (require 'ycmd)
; (add-hook 'after-init-hook #'global-ycmd-mode)
; (require 'company-ycmd)
; (company-ycmd-setup)

; (require 'magit)
; (setq magit-auto-revert-mode nil)
; (setq magit-last-seen-setup-instructions "1.4.0")
; ; (evil-leader/set-key "ug" 'magit-status)

; ;; Save session including tabs
; ;; http://stackoverflow.com/questions/22445670/save-and-restore-elscreen-tabs-and-split-frames
; (defun session-save ()
;     "Store the elscreen tab configuration."
;     (interactive)
;     (if (desktop-save emacs-configuration-directory)
;         (with-temp-file elscreen-tab-configuration-store-filename
;             (insert (prin1-to-string (elscreen-get-screen-to-name-alist))))))

; ;; Load session including tabs
; (defun session-load ()
;     "Restore the elscreen tab configuration."
;     (interactive)
;     (if (desktop-read)
;         (let ((screens (reverse
;                         (read
;                          (with-temp-buffer
;                           (insert-file-contents elscreen-tab-configuration-store-filename)
;                           (buffer-string))))))
;             (while screens
;                 (setq screen (car (car screens)))
;                 (setq buffers (split-string (cdr (car screens)) ":"))
;                 (if (eq screen 0)
;                     (switch-to-buffer (car buffers))
;                     (elscreen-find-and-goto-by-buffer (car buffers) t t))
;                 (while (cdr buffers)
;                     (switch-to-buffer-other-window (car (cdr buffers)))
;                     (setq buffers (cdr buffers)))
;                 (setq screens (cdr screens))))))

; (require 'iso-transl)

; ;; "after" macro definition
; (if (fboundp 'with-eval-after-load)
;     (defmacro after (feature &rest; body)
;       "After FEATURE is loaded, evaluate BODY."
;       (declare (indent defun))
;       `(with-eval-after-load ,feature ,@body))
;   (defmacro after (feature &rest; body)
;     "After FEATURE is loaded, evaluate BODY."
;     (declare (indent defun))
;     `(eval-after-load ,feature
;        '(progn ,@body))))))

; (require 'evil-search-highlight-persist)
; (global-evil-search-highlight-persist t)
