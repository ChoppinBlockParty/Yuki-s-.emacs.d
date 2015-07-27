 ; ; (eval-after-load "tex"
 ; ;   '(remove-from-list 'TeX-command-list
 ; ; 		'("Latex Make" "latexmk %(-pdf) %t" TeX-run-TeX) t)
 ; ;   )

; ; (eval-when-compile
; ;   (require 'cl))
; ; (load "auctex.el" nil t t)

; ; (require 'tex)

 ; ; (use-package tex-site
 ; ;   :ensure auctex
 ; ;   :config
 ; ;   (progn
 ; ; (use-package preview-latex
 ; ;   :ensure t
 ; ;   :config
 ; ;   (progn))

 ;     ; (setq-default TeX-master nil)
 ;     ;; (setq-default TeX-command-default "Latex Make")
 
 ;     ; (setq TeX-view-program-list '(("zathura" "zathura -s -x \"emacsclient --no-wait +%%{line} %%{input}\" %o")))
 ;     ;; Always use PDF mode
 ;     ; (setq TeX-PDF-mode t)
 
 ;     ;; View my PDFs with Zathura
 ;     ; (setq TeX-view-program-selection '((output-pdf "zathura")))

 ;    ;; yasnippet code 'optional', before auto-complete
 ;    ; (use-package yasnippet
 ;    ;   :ensure t
 ;    ;   :config
 ;    ;   (progn
 ;    ;     (yas-global-mode 1)
 ;    ;   )
 ;    ; )
 ;    ; (use-package company-auctex
 ;    ;   :ensure t
 ;    ;   :config
 ;    ;   (progn
 ;    ;     (company-auctex-init)
 ;    ;   )
 ;    ; )

 ;    ;; auto-complete setup, sequence is important
 ;    ; (require 'auto-complete)
 ;    ; (add-to-list 'ac-modes 'latex-mode) ; beware of using 'LaTeX-mode instead
 ;    ; (require 'ac-math) ; package should be installed first 
 ;    ; (defun my-ac-latex-mode () ; add ac-sources for latex
 ;    ;    (setq ac-sources
 ;    ;          (append '(ac-source-math-unicode
 ;    ;            ac-source-math-latex
 ;    ;            ac-source-latex-commands)
 ;    ;                  ac-sources)))
 ;    ; (add-hook 'LaTeX-mode-hook 'my-ac-latex-mode)
 ;    ; (setq ac-math-unicode-in-math-p t)
 ;    ; (ac-flyspell-workaround) ; fixes a known bug of delay due to flyspell (if it is there)
 ;    ; (add-to-list 'ac-modes 'org-mode) ; auto-complete for org-mode (optional)
 ;    ; (require 'auto-complete-config) ; should be after add-to-list 'ac-modes and hooks
 ;    ; (ac-config-default)
 ;    ; (setq ac-auto-start nil)            ; if t starts ac at startup automatically
 ;    ; (setq ac-auto-show-menu t)
 ;    ; (global-company-mode t)

 ;     ;; Maybe someday this will actually work and I can sync PDFs with Zathura
 ;     ; (setq TeX-source-correlate-mode t)
 ;     ; (setq TeX-source-correlate-method 'synctex)
 ;     ; (setq TeX-source-correlate-start-server t)
 ;     (setq TeX-auto-save t)
 ;     ; (setq TeX-parse-self t)
 ;     ; (setq-default TeX-master nil)
 ;     (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
 ;     (add-hook 'LaTeX-mode-hook 'flyspell-mode)
 ;     (add-hook 'LaTeX-mode-hook 'flyspell-buffer)
 ;     ; (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
 ;     ; (add-hook 'LaTeX-mode-hook 'orgtbl-mode)
 ;     (add-hook 'TeX-mode-hook (lambda () (TeX-fold-mode 1))); Automatically activate TeX-fold-mode.
 
 ;     ;; source:
 ;     ;; http://tex.stackexchange.com/questions/185688/how-to-force-emacs-with-auctex-to-show-compilation-in-new-buffer
 ;     ; (add-to-list 'TeX-command-list '("pdfLaTeX" "pdflatex -shell-escape %t" TeX-run-interactive nil t))
 ;     ; (defcustom tex-my-viewer "zathura --fork -s -x \"emacsclient --eval '(progn (switch-to-buffer  (file-name-nondirectory \"'\"'\"%{input}\"'\"'\")) (goto-line %{line}))'\"" 
 ;     ;   "PDF Viewer for TeX documents. You may want to fork the viewer
 ;     ; so that it detects when the same document is launched twice, and
 ;     ; persists when Emacs gets closed.
 
 ;     ; Simple command:
 
 ;     ;   zathura --fork
 
 ;     ; We can use
 
 ;     ;   emacsclient --eval '(progn (switch-to-buffer  (file-name-nondirectory \"%{input}\")) (goto-line %{line}))'
 
 ;     ; to reverse-search a pdf using SyncTeX. Note that the quotes and double-quotes matter and must be escaped appropriately."
 ;     ;   :safe 'stringp)
 
 ;     (use-package auctex-latexmk
 ;       :ensure auctex-latexmk
 ;       :init
 ;       (progn
 ;         (auctex-latexmk-setup)
 ;         )
 ;     )
 ;  )
; )
(provide 'latex-config)

