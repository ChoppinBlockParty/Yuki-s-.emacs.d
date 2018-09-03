;; show-paren-mode allows one to see matching pairs of parentheses and other characters. When point is on the opening character of one of the paired characters, the other is highlighted. When the point is after the closing character of one of the paired characters, the other is highlighted. 

(show-paren-mode t)
(setq show-paren-delay 0)
;; (setq show-paren-style 'expression)
(setq show-paren-style 'parenthesis)

(provide 'show-paren-mode-config)
