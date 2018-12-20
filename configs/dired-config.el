;;; dired-config --- Genuinely dire
;;; Commentary:
;;; Code:

(use-package dired
  :ensure nil
  :config
  (setq
    dired-listing-switches "-Ahl --group-directories-first"
    )

  ;;; When you choose a directory to visit, it is normally visited in a new buffer – the
  ;;; Dired buffer you chose it in is not deleted.
  ;;; Disable this behavior by disabling `dired-find-alternate-file`.
  (put 'dired-find-alternate-file 'disabled nil)

  (defun my-dired-up-directory ()
    "Take dired up one directory, but behave like dired-find-alternate-file."
    (interactive)
    (let ((old (current-buffer)))
      (dired-up-directory)
      (kill-buffer old)))

  (defun my-dired-create-file (file)
    "Create a file called FILE. If FILE already exists, signal an error."
    (interactive (list (read-file-name "Create file: " (dired-current-directory))))
    (let* ((expanded (expand-file-name file))
           (try expanded)
           (dir (directory-file-name (file-name-directory expanded)))
           new)
          (if (file-exists-p expanded)
              (error "Cannot create file %s: file exists" expanded)
              ;; Find the topmost nonexistent parent dir (variable `new')
              (while (and try (not (file-exists-p try)) (not (equal new try)))
                (setq new try
                      try (directory-file-name (file-name-directory try))))
              (when (not (file-exists-p dir)) (make-directory dir t))
              (write-region "" nil expanded t)
              (when new
                (dired-add-file new)
                (my-dired-revert)
                )
              )
          )
    )

  (defun my-dired-create-directory (directory)
    "Create a directory called DIRECTORY.
  Parent directories of DIRECTORY are created as needed.
  If DIRECTORY already exists, signal an error."
    (interactive
     (list (read-file-name "Create directory: " (dired-current-directory))))
    (let* ((expanded (directory-file-name (expand-file-name directory)))
           (try expanded)
           new)
      (if (file-exists-p expanded)
          (error "Cannot create directory %s: file exists" expanded))
          ;; Find the topmost nonexistent parent dir (variable `new')
          (while (and try (not (file-exists-p try)) (not (equal new try)))
            (setq new try
                  try (directory-file-name (file-name-directory try)))
            )
          (make-directory expanded t)
          (when new
                (dired-add-file new)
                (my-dired-revert)
                )
      )
    )

  (evil-define-key 'normal dired-mode-map
      "q" 'quit-window
      "j" 'dired-next-line
      "k" 'dired-previous-line
      "C" 'dired-do-copy
      "D" 'dired-do-delete
      "R" 'dired-do-rename
      "zc" 'dired-diff
      "zd" 'dired-hide-details-mode
      "zM" 'dired-do-chmod
      "zO" 'dired-do-chown
      "zt" 'dired-toggle-marks
      "z." 'dired-mark-extension
      "s"  'dired-mark
      "u"  'dired-unmark
      "U"  'dired-unmark-all-marks
      (kbd "C-m") 'dired-find-file
      "zww" 'dired-toggle-read-only
      "go" 'browse-url-of-dired-file
      "ga" 'dired-show-file-type ;; FIXME: This could probably go on a better key.
      "gg" 'evil-goto-first-line ;; FIXME: Do not why I should specify it here, otherwise it is undefined
      "gf" 'find-file
      "gn" 'my-dired-create-file
      "gN" 'my-dired-create-directory
      "gr" 'my-dired-revert
      "Y"  'dired-copy-filename-as-kill
      "o" (lambda () (interactive) (dired-subtree-toggle) (my-dired-revert))
      ;; "o" 'dired-subtree-toggle
      ;; "o" 'dired-subtree-toggle
      (kbd "<return>") 'dired-find-file
      (kbd "S-<return>") 'dired-display-file ;; preview
      (kbd "M-<return>") 'dired-find-file-other-window
      "[" 'dired-prev-dirline
      "]" 'dired-next-dirline
      "<" 'my-dired-up-directory
      ">" 'dired-view-file
      [?\S-\ ] 'dired-previous-line
      [remap next-line] 'dired-next-line
      [remap previous-line] 'dired-previous-line
      ;; hiding
      "g$" 'dired-hide-subdir ;; FIXME: This can probably live on a better binding.
      (kbd "M-$") 'dired-hide-all
      "(" 'dired-hide-details-mode
      ;; isearch
      (kbd "M-s a C-s")   'dired-do-isearch
      (kbd "M-s a M-C-s") 'dired-do-isearch-regexp
      (kbd "M-s f C-s")   'dired-isearch-filenames
      (kbd "M-s f M-C-s") 'dired-isearch-filenames-regexp
      ;; misc
      [remap read-only-mode] 'dired-toggle-read-only
      "g?" 'dired-summary
      (kbd "<delete>") 'dired-unmark-backward
      [remap undo] 'dired-undo
      [remap advertised-undo] 'dired-undo
      ;; thumbnail manipulation (image-dired)
      (kbd "C-t d") 'image-dired-display-thumbs
      (kbd "C-t t") 'image-dired-tag-files
      (kbd "C-t r") 'image-dired-delete-tag
      (kbd "C-t j") 'image-dired-jump-thumbnail-buffer
      (kbd "C-t i") 'image-dired-dired-display-image
      (kbd "C-t x") 'image-dired-dired-display-external
      (kbd "C-t a") 'image-dired-display-thumbs-append
      (kbd "C-t .") 'image-dired-display-thumb
      (kbd "C-t c") 'image-dired-dired-comment-files
      (kbd "C-t f") 'image-dired-mark-tagged-files
      (kbd "C-t C-t") 'image-dired-dired-toggle-marked-thumbs
      (kbd "C-t e") 'image-dired-dired-edit-comment-and-tags
      ;; encryption and decryption (epa-dired)
      ;; ";d" 'epa-dired-do-decrypt
      ;; ";v" 'epa-dired-do-verify
      ;; ";s" 'epa-dired-do-sign
      ;; ";e" 'epa-dired-do-encrypt
      [mouse-2] 'dired-mouse-find-file-other-window
      [follow-link] 'mouse-face
      )
  )

(use-package wdired
  :ensure nil
  :init
  (defvar wdired-mode-map
    (let ((map (make-sparse-keymap)))
      map)
    "Keymap used in `wdired-mode'.")
  :config
  (setq
    wdired-allow-to-change-permissions t
    )
  (evil-set-initial-state 'wdired-mode 'normal)

  (evil-define-key nil wdired-mode-map [remap evil-write] 'wdired-finish-edit)

  (evil-define-key 'normal wdired-mode-map
    "ZQ" 'wdired-abort-changes
    "ZZ" 'wdired-finish-edit
    (kbd "<escape>") 'wdired-exit))

(use-package dired-subtree
  :config
  (defun my-dired-subtree-prefix(depth)
    (let ((str ""))
         (while (< 0 depth)
                (setq
                  str   (concat str "●-")
                  depth (1- depth)
                  )
                )
         str)
    )
  (setq
    dired-subtree-line-prefix 'my-dired-subtree-prefix
    )
  )
(use-package dired-collapse)

(use-package dired-sidebar
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (setq
    dired-sidebar-disable-dired-collapse nil
    dired-sidebar-theme 'none
    dired-sidebar-refresh-on-projectile-switch nil
    ;; dired-sidebar-use-term-integration t
    ;; dired-sidebar-use-custom-font t
    )
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (define-key evil-normal-state-map "`" 'dired-sidebar-toggle-sidebar)
  )

(defface all-the-icons-dired-dir-face
  '((((background dark)) :foreground "white")
    (((background light)) :foreground "black"))
  "Face for the directory icon"
  :group 'all-the-icons-faces)

(defcustom all-the-icons-dired-v-adjust 0.01
  "The default vertical adjustment of the icon in the dired buffer."
  :group 'all-the-icons
  :type 'number)

(defvar-local all-the-icons-dired-displayed nil
  "Flags whether icons have been added.")

(defun all-the-icons-dired--display ()
  "Display the icons of files in a dired buffer."
  (when (and (not all-the-icons-dired-displayed) dired-subdir-alist)
    (setq-local all-the-icons-dired-displayed t)
    (let ((inhibit-read-only t)
          (remote-p (and (fboundp 'tramp-tramp-file-p) (tramp-tramp-file-p default-directory)))
          (max (1- (dired-subdir-max)))
          last
          )
          (save-excursion
            (goto-char (point-min))
            (setq last (point))
            (dired-goto-next-file)
            (while (and (not (= (point) last))
                        (< (point) max))
              (setq last (point))
              (let ((filename (dired-get-filename 'verbatim t)))
                (unless (member filename '("." ".."))
                  (let ((filepath (dired-get-filename nil t)))
                    (cond ((file-directory-p filepath)
                           (let* ((matcher (all-the-icons-match-to-alist filename all-the-icons-dir-icon-alist))
                                  (icon (cond
                                    (remote-p
                                    (all-the-icons-octicon "file-directory" :v-adjust all-the-icons-dired-v-adjust :face 'all-the-icons-dired-dir-face))
                                    ((file-symlink-p filepath)
                                    (all-the-icons-octicon "file-symlink-directory" :v-adjust all-the-icons-dired-v-adjust :face 'all-the-icons-dired-dir-face))
                                    ((all-the-icons-dir-is-submodule filepath)
                                    (all-the-icons-octicon "file-submodule" :v-adjust all-the-icons-dired-v-adjust :face 'all-the-icons-dired-dir-face))
                                    ((file-exists-p (format "%s/.git" filepath))
                                    (all-the-icons-octicon "repo" :v-adjust all-the-icons-dired-v-adjust :face 'all-the-icons-dired-dir-face))
                                    (t (apply (car matcher) (list (cadr matcher) :face 'all-the-icons-dired-dir-face :v-adjust all-the-icons-dired-v-adjust))))))
                                  (insert (concat icon " "))
                             )
                           )
                          ((file-exists-p filepath)
                           (insert (concat (all-the-icons-icon-for-file filename) " "))
                           )
                          (t nil)
                          )
                    )
                  )
                )
              (forward-line 1)
              ;;; Buffer has changed since we inserted an icon, update `max`
              (setq max (1- (dired-subdir-max)))
              (while (and (not (dired-move-to-filename)) (< (point) max))
                (forward-line 1))
              )
            )
          )
    )
  )

(defun all-the-icons-dired--reset (&optional _arg _noconfirm)
  "Functions used as advice when redisplaying buffer."
  (setq-local all-the-icons-dired-displayed nil))

(defun my-dired-revert (&rest _)
  "Wrapper around `dired-revert' but saves window position."
  (dired-sidebar-when-let* ((win (get-buffer-window (current-buffer))))
    (with-selected-window win
      (let ((old-window-start (window-start)))
        (when (dired-sidebar-using-tui-p)
          (dired-sidebar-tui-reset-in-sidebar))
        (dired-revert)
        (set-window-start win old-window-start)))))

(defun my-dired-mode-hook()
  "Hook to setup all-the-icons."
  (dired-hide-details-mode 1)
  ;; (dired-collapse-mode 1)
  (add-hook 'dired-after-readin-hook 'all-the-icons-dired--display t t)
  )
(advice-add 'dired-revert :before #'all-the-icons-dired--reset)
(add-hook 'dired-mode-hook #'my-dired-mode-hook)


(provide 'dired-config)
;;; dired-config.el ends here
