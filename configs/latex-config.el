;;; latex-config --- ivy
;;; Commentary:
;;; Code:
(setq TeX-auto-save t)
; (setq TeX-parse-self t)
; (setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-buffer)
(provide 'latex-config)

