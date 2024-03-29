;;; flyspell-config --- Configures spell checkers
;;; Commentary:
;;; Code:

(with-eval-after-load 'ispell

(use-package flyspell
  :config
  (setq
    flyspell-mode-line-string ""
    ;;; Disable string spell check
    flyspell-prog-text-faces (delq 'font-lock-string-face flyspell-prog-text-faces)
    ;; better performance
    flyspell-issue-message-flag nil
  )
 )

;; if (aspell installed) { use aspell}
;; else if (hunspell installed) { use hunspell }
;; whatever spell checker I use, I always use English dictionary
;; I prefer use aspell because:
;; 1. aspell is older
;; 2. looks Kevin Atkinson still get some road map for aspell:
;; @see http://lists.gnu.org/archive/html/aspell-announce/2011-09/msg00000.html
(defun flyspell-detect-ispell-args (&optional RUN-TOGETHER)
  "if RUN-TOGETHER is true, spell check the CamelCase words"
  (let (args)
    (when ispell-program-name
      (cond
        ((string-match "aspell$" ispell-program-name)
         ;; force the English dictionary, support Camel Case spelling check (tested with aspell 0.6)
         (setq args (list "--sug-mode=ultra" "--lang=en_US"))
         (if RUN-TOGETHER
           (setq args (append args '("--run-together" "--run-together-limit=16" "--run-together-min=2")))))
        ((string-match "hunspell$" ispell-program-name)
         (setq args (list "-d" "en_US")))))
    args
    ))

;; Aspell Setup: Makes emacs to freeze
;;
;; Hunspell Setup:
;; 1. Install hunspell from http://hunspell.sourceforge.net/
;; 2. Download openoffice dictionary extension from
;; http://extensions.openoffice.org/en/project/english-dictionaries-apache-openoffice
;; 3. That is download `dict-en.oxt'. Rename that to `dict-en.zip' and unzip
;; the contents to a temporary folder.
;; 4. Copy `en_US.dic' and `en_US.aff' files from there to a folder where you
;; save dictionary files; I saved it to `~/usr_local/share/hunspell/'
;; 5. Add that path to shell env variable `DICPATH':
;; setenv DICPATH $MYLOCAL/share/hunspell
;; 6. Restart emacs so that when hunspell is run by ispell/flyspell, that env
;; variable is effective.
;;
;; hunspell will search for a dictionary called `en_US' in the path specified by
;; `$DICPATH'

;; (cond
;;  ((executable-find "hunspell")
  (setq ispell-program-name (executable-find "hunspell"))
  ;; just reset dictionary to the safe one "en_US" for hunspell.
  ;; if we need use different dictionary, we specify it in command line arguments
  ;; (setq ispell-dictionary "en_US")
  ;; (setq ispell-really-hunspell t)
  (setq ispell-local-dictionary "en_US")
  ;; (setq ispell-local-dictionary-alist
  ;;       '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8)))
 ;; )
 ;; ((executable-find "aspell")
 ;;  (setq ispell-program-name "aspell"))
 ;; (t (setq ispell-program-name nil)))


;; (add-to-list 'ispell-local-dictionary-alist '("deutsch-hunspell"
;;                                               "[[:alpha:]]"
;;                                               "[^[:alpha:]]"
;;                                               "[']"
;;                                               t
;;                                               ("-d" "de_DE"); Dictionary file name
;;                                               nil
;;                                               iso-8859-1))

;; (add-to-list 'ispell-local-dictionary-alist '("english-hunspell"
;;                                               "[[:alpha:]]"
;;                                               "[^[:alpha:]]"
;;                                               "[']"
;;                                               t
;;                                               ("-d" "en_US")
;;                                               nil
;;                                               iso-8859-1))

;; ispell-cmd-args is useless, it's the list of *extra* command line arguments we will append to the ispell process when ispell-send-string()
;; ispell-extra-args is the command arguments which will *always* be used when start ispell process
(setq ispell-extra-args (flyspell-detect-ispell-args t))
(setq ispell-cmd-args (flyspell-detect-ispell-args))

(defadvice ispell-word (around my-ispell-word activate)
  (let ((old-ispell-extra-args ispell-extra-args))
    (ispell-kill-ispell t)
    ;; use emacs original arguments
    (setq ispell-extra-args (flyspell-detect-ispell-args))
    ad-do-it
    ;; restore our own ispell arguments
    (setq ispell-extra-args old-ispell-extra-args)
    (ispell-kill-ispell t)
    ))

(defadvice flyspell-auto-correct-word (around my-flyspell-auto-correct-word activate)
  (let ((old-ispell-extra-args ispell-extra-args))
    (ispell-kill-ispell t)
    ;; use emacs original arguments
    (setq ispell-extra-args (flyspell-detect-ispell-args))
    ad-do-it
    ;; restore our own ispell arguments
    (setq ispell-extra-args old-ispell-extra-args)
    (ispell-kill-ispell t)
    ))

;; Add auto spell-checking in comments for all programming language modes
;; if and only if there is enough memory
;; You can use prog-mode-hook instead.
(dolist (hook my-prog-modes-hook-list)
  (add-hook hook 'flyspell-prog-mode))
(dolist (hook my-markup-modes-hook-list)
  (add-hook hook 'flyspell-mode))


(define-key evil-normal-state-map (kbd "z C-=") 'flyspell-auto-correct-word)

)

(provide 'flyspell-config)
;;; flyspell-config.el ends here
