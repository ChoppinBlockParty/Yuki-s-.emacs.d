;; requires **virtualenv** to work
;; pip install --upgrade /home/qwert/.emacs.d/elpa/jedi-core-20150623.2335
(use-package jedi
  :ensure t
  :config
  (progn

    (add-hook 'python-mode-hook 'jedi:setup)

    (after 'company
    (use-package company-jedi
    :ensure t
    :config
      (progn
        (defun my/python-mode-hook ()
        (add-to-list 'company-backends 'company-jedi))

        (add-hook 'python-mode-hook 'my/python-mode-hook)
      )
    )
    )
  )
)

(provide 'jedi-config)
