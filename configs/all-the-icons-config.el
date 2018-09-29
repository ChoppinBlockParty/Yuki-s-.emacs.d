;;; all-the-icons --- Wonderful icon package
;;; Commentary:
;;; Code:
(use-package all-the-icons
  :config
  (setq all-the-icons-mode-icon-alist
    (append all-the-icons-mode-icon-alist
            '((sh-mode all-the-icons-alltheicon "terminal" :face all-the-icons-lblue))
            '((vimrc-mode all-the-icons-faicon "vimeo" :face all-the-icons-lblue))
            '((gitattributes-mode all-the-icons-alltheicon "git" :face all-the-icons-lblue))
            '((gitconfig-mode all-the-icons-alltheicon "git" :face all-the-icons-lblue))
            '((gitignore-mode all-the-icons-alltheicon "git" :face all-the-icons-lblue))
            )
    )
  )
(provide 'all-the-icons-config)
;;; all-the-icons-config.el ends here
