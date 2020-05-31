;;; all-the-icons --- Wonderful icon package
;;;
;;; Commentary:
;;;   https://github.com/domtronn/all-the-icons.el
;;;   Installing Fonts
;;;
;;;   In order for the icons to work it is very important that you install the Resource Fonts included in this package, they are available in the fonts directory. You can also install the latest fonts for this package in the (guessed?) based on the OS by calling the following function;
;;;
;;;   M-x all-the-icons-install-fonts
;;;
;;;   Bear in mind, this will also run fc-cache -f -v on MacOS and Linux which can take some time to complete. For Windows, this function will prompt for a download directory for you to install them manually.
;;;
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
