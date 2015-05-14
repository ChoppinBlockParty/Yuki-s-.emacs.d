;; /This/ file (~init.el~) that you are reading
;; should be in this folder
(add-to-list 'load-path "~/.emacs.d/")

;; Package Manager
;; See ~Cask~ file for its configuration
;; https://github.com/cask/cask
(require 'cask "~/.emacs.d/cask/cask.el")
(cask-initialize)

;; Keeps ~Cask~ file in sync with the packages
;; that you install/uninstall via ~M-x list-packages~
;; https://github.com/rdallasgray/pallet
(require 'pallet)

;; Root directory
(setq root-dir (file-name-directory
                (or (buffer-file-name) load-file-name)))

(require 'evil)
(evil-mode 1)

(defun require-package (package)
  (setq-default highlight-tabs t)
  "Install given PACKAGE."
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(setq molokai-theme-kit t)
(load-theme 'molokai t)

;; Don't show startup screen
(setq inhibit-startup-screen t)
; (load-theme 'misterioso t)
; (menu-bar-mode -1)
; (tool-bar-mode -1)
; (scroll-bar-mode -1)
(color-theme-approximate-on)
(global-linum-mode 1)
(cua-mode t)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
    (transient-mark-mode 1) ;; No region when it is not highlighted
    (setq cua-keep-region-after-copy t) ;; Standard Windows behaviour

(cua-selection-mode t)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(transient-mark-mode 1) ;; No region when it is not highlighted
(setq cua-keep-region-after-copy t) ;; Standard Windows behaviour
(setq x-select-enable-clipboard t)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
    (transient-mark-mode 1) ;; No region when it is not highlighted
    (setq cua-keep-region-after-copy t) ;; Standard Windows behaviour
(setq evil-emacs-state-cursor '("red" box))
(setq evil-normal-state-cursor '("green" box))
(setq evil-visual-state-cursor '("orange" box))
(setq evil-insert-state-cursor '("red" bar))
(setq evil-replace-state-cursor '("red" bar))
(setq evil-operator-state-cursor '("red" hollow))

(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(setq evil-leader/in-all-states 1)

(setq evil-auto-indent t)
(setq evil-shift-width 2)
(setq evil-regexp-search t)
(setq evil-want-C-i-jump t)

; (define-key evil-normal-state-map "5" 'evil-end-of-line)
; (define-key evil-normal-state-map "%" '5)
; (define-key evil-normal-state-map "8" 'evil-end-of-line)
; (define-key evil-normal-state-map "*" 'digit-argument)

(define-key evil-normal-state-map "\\" 'evil-window-vsplit)
(define-key evil-normal-state-map "-" 'evil-window-split)
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

(require 'helm)
(require 'helm-config)
(defhydra helm-like-unite (:hint nil
                           :color pink)
  "
Nav ^^^^^^^^^        Mark ^^          Other ^^       Quit
^^^^^^^^^^------------^^----------------^^----------------------
_K_ ^ ^ _k_ ^ ^     _m_ark           _v_iew         _i_: cancel
^↕^ _h_ ^✜^ _l_     _t_oggle mark    _H_elp         _o_: quit
_J_ ^ ^ _j_ ^ ^     _U_nmark all     _d_elete
^^^^^^^^^^                           _f_ollow: %(helm-attr 'follow)
"
  ;; arrows
  ("h" helm-beginning-of-buffer)
  ("j" helm-next-line)
  ("k" helm-previous-line)
  ("l" helm-end-of-buffer)
  ;; beginning/end
  ("g" helm-beginning-of-buffer)
  ("G" helm-end-of-buffer)
  ;; scroll
  ("K" helm-scroll-other-window-down)
  ("J" helm-scroll-other-window)
  ;; mark
  ("m" helm-toggle-visible-mark)
  ("t" helm-toggle-all-marks)
  ("U" helm-unmark-all)
  ;; exit
  ("<escape>" keyboard-escape-quit "" :exit t)
  ("o" keyboard-escape-quit :exit t)
  ("i" nil)
  ;; sources
  ("}" helm-next-source)
  ("{" helm-previous-source)
  ;; rest
  ("H" helm-help)
  ("v" helm-execute-persistent-action)
  ("d" helm-persistent-delete-marked)
  ("f" helm-follow-mode))

(define-key helm-map (kbd "<escape>") 'helm-like-unite/body)
(define-key helm-map (kbd "C-k") 'helm-like-unite/body)
(define-key helm-map (kbd "C-o") 'helm-like-unite/body)

(defun helm-persistent-delete-marked ()
  "Kill buffer without quitting helm."
  (interactive)
  (if (equal (cdr (assoc 'name (helm-get-current-source)))
             "Buffers")
      (with-helm-alive-p
        (helm-attrset 'kill-action
                      '(helm-persistent-kill-buffers . never-split))
        (helm-execute-persistent-action 'kill-action))
    (user-error "Only works for buffers")))

(defun helm-persistent-kill-buffers (_buffer)
  (unwind-protect
       (dolist (b (helm-marked-candidates))
         (helm-buffers-persistent-kill-1 b))
    (with-helm-buffer
      (setq helm-marked-candidates nil
            helm-visible-mark-overlays nil))
    (helm-force-update (helm-buffers--quote-truncated-buffer
                        (helm-get-selection)))))

(define-key evil-normal-state-map "`" (lambda ()
    (interactive)
    (dired "~/")))
(define-key evil-normal-state-map "\M-q" (lambda ()
    (interactive)
    (dired "~/")))
(define-key evil-normal-state-map "\M-a" (lambda ()
    (interactive)
    (dired "~/")))
(define-key evil-normal-state-map "\M-w" 'helm-buffers-list)
(define-key evil-normal-state-map "\M-s" 'helm-buffers-list)
(define-key evil-normal-state-map "\M-e" 'helm-for-files)
(define-key evil-normal-state-map "\M-d" 'helm-for-files)

(evil-leader/set-key
        "f" 'helm-for-files
        ; "l" 'helm-locate
        "c" 'helm-colors
        "y" 'helm-show-kill-ring
        "t" 'helm-top
        "m" 'helm-man-woman
        "o" 'helm-occur
        ; "j" 'helm-M-x
        "e" 'helm-find-files
        "b" 'helm-buffers-list
        ; "h" 'helm-projectile-find-file
        "r" 'helm-recentf
        ; "H" 'helm-projectile
        )

(require 'auto-complete-config)
(require 'flx-ido)
(flx-ido-mode 1)
(setq flx-ido-threshhold 300)

(require 'magit)
(setq magit-auto-revert-mode nil)
(setq magit-last-seen-setup-instructions "1.4.0")
(evil-leader/set-key "ug" 'magit-status)

(define-key ac-mode-map (kbd "M-/") 'ac-fuzzy-complete)
(dolist (ac-mode '(text-mode org-mode))
  (add-to-list 'ac-modes ac-mode))
(dolist (ac-mode-hook '(text-mode-hook org-mode-hook prog-mode-hook))
  (add-hook ac-mode-hook
              (lambda ()
                (setq ac-fuzzy-enable t)
                (add-to-list 'ac-sources 'ac-source-files-in-current-dir)
                (add-to-list 'ac-sources 'ac-source-filename))))

(ac-config-default)

;; Save session including tabs
;; http://stackoverflow.com/questions/22445670/save-and-restore-elscreen-tabs-and-split-frames
(defun session-save ()
    "Store the elscreen tab configuration."
    (interactive)
    (if (desktop-save emacs-configuration-directory)
        (with-temp-file elscreen-tab-configuration-store-filename
            (insert (prin1-to-string (elscreen-get-screen-to-name-alist))))))

;; Load session including tabs
(defun session-load ()
    "Restore the elscreen tab configuration."
    (interactive)
    (if (desktop-read)
        (let ((screens (reverse
                        (read
                         (with-temp-buffer
                          (insert-file-contents elscreen-tab-configuration-store-filename)
                          (buffer-string))))))
            (while screens
                (setq screen (car (car screens)))
                (setq buffers (split-string (cdr (car screens)) ":"))
                (if (eq screen 0)
                    (switch-to-buffer (car buffers))
                    (elscreen-find-and-goto-by-buffer (car buffers) t t))
                (while (cdr buffers)
                    (switch-to-buffer-other-window (car (cdr buffers)))
                    (setq buffers (cdr buffers)))
                (setq screens (cdr screens))))))

(require 'iso-transl)

;; "after" macro definition
(if (fboundp 'with-eval-after-load)
    (defmacro after (feature &rest; body)
      "After FEATURE is loaded, evaluate BODY."
      (declare (indent defun))
      `(with-eval-after-load ,feature ,@body))
  (defmacro after (feature &rest; body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))))

(require 'evil-search-highlight-persist)
(global-evil-search-highlight-persist t)
