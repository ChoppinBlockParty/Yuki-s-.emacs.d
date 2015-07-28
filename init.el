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

; (require 'crosshairs)

; (use-package column-marker
;   :ensure t
;   :demand
;   :config
;   (progn
;   )
; )

(global-hl-line-mode 1)

;; To customize the background color
(set-face-background 'hl-line "#330")  ;; Emacs 22 Only

(use-package highlight-symbol
  :ensure t
  :config
  (progn
    (global-set-key [(f2)] 'highlight-symbol)
    (global-set-key [f3] 'highlight-symbol-next)
    (global-set-key [(shift f3)] 'highlight-symbol-prev)
    (global-set-key [(meta f3)] 'highlight-symbol-query-replace)
  )
)

(require 'configs-base)
(require 'auto-fill-mode-config)
; (require 'crosshairs-config)
(require 'config-color)
(require 'config-cl-lib)
(require 'configs-evil)
(require 'configs-dired)
(require 'configs-helm)
(require 'configs-projectile)
; (require 'config-sunrise-commander)
; (require 'ranger-config)
(require 'config-paren)
(require 'config-rainbow-delimiters)
(require 'config-autocompletion)
(require 'config-cider)
(require 'config-easymotion)
(require 'ido-config)
(require 'cua-config)
(require 'spell-check-config)
; (require 'latex-config)


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
