;;; dired-config --- Genuinely dire
;;; Commentary:
;;; Code:

(use-package dired
  :ensure nil
  :init
  (defvar dired-mode-map
    ;; This looks ugly when substitute-command-keys uses C-d instead d:
    ;;  (define-key dired-mode-map "\C-d" 'dired-flag-file-deletion)
    (let ((map (make-keymap)))
      (set-keymap-parent map special-mode-map)
      map)
    "Local keymap for Dired mode buffers.")
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
                (dired-revert)
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
                (dired-revert)
                )
      )
    )

  (evil-define-key 'normal dired-mode-map
      "q" 'quit-window
      "j" 'dired-next-line
      "k" 'dired-previous-line
      [mouse-2] 'dired-mouse-find-file-other-window
      [follow-link] 'mouse-face
      ;; Commands to mark or flag certain categories of files
      ;; "#" 'dired-flag-auto-save-files
      ;; "." 'dired-clean-directory
      ;; "~" 'dired-flag-backup-files
      ;;; Byte compile marked (or next ARG) Emacs Lisp files.
      ;; "B" 'dired-do-byte-compile
      ;; "A" 'dired-do-find-regexp
      "C" 'dired-do-copy
      "D" 'dired-do-delete
      "R" 'dired-do-rename
      ;; "gG" 'dired-do-chgrp ;; FIXME: This can probably live on a better binding.
      ;; "H" 'dired-do-hardlink
      ;; "L" 'dired-do-load
      "zM" 'dired-do-chmod
      "zO" 'dired-do-chown
      ;; "P" 'dired-do-print
      ;; "Q" 'dired-do-find-regexp-and-replace
      ;; "S" 'dired-do-symlink
      ;; "T" 'dired-do-touch
      ;; "X" 'dired-do-shell-command
      ;; "Z" 'dired-do-compress
      ;; "c" 'dired-do-compress-to
      ;; "!" 'dired-do-shell-command
      ;; "&" 'dired-do-async-shell-command
      ;; Comparison commands
      "zc" 'dired-diff
      ;; Tree Dired commands
      ;; (kbd "M-C-?") 'dired-unmark-all-files
      ;; (kbd "M-C-d") 'dired-tree-down
      ;; (kbd "M-C-u") 'dired-tree-up
      ;; (kbd "M-C-n") 'dired-next-subdir
      ;; (kbd "M-C-p") 'dired-prev-subdir
      ;; move to marked files
      ;; (kbd "M-{") 'dired-prev-marked-file
      ;; (kbd "M-}") 'dired-next-marked-file
      ;; Make all regexp commands share a `%' prefix:
      ;; We used to get to the submap via a symbol dired-regexp-prefix,
      ;; but that seems to serve little purpose, and copy-keymap
      ;; does a better job without it.
      ;; "1" nil
      ;; "1u" 'dired-upcase
      ;; "1l" 'dired-downcase
      ;; "1d" 'dired-flag-files-regexp
      ;; "1g" 'dired-mark-files-containing-regexp
      ;; "1m" 'dired-mark-files-regexp
      ;; "1r" 'dired-do-rename-regexp
      ;; "1C" 'dired-do-copy-regexp
      ;; "1H" 'dired-do-hardlink-regexp
      ;; "1R" 'dired-do-rename-regexp
      ;; "1S" 'dired-do-symlink-regexp
      ;; "1&" 'dired-flag-garbage-files
      ;; mark
      "z" nil
      ;; "z*" 'dired-mark-executables
      ;; "z/" 'dired-mark-directories
      ;; "z@" 'dired-mark-symlinks
      ;; "z%" 'dired-mark-files-regexp
      ;; "z(" 'dired-mark-sexp
      "z." 'dired-mark-extension
      ;; "zO" 'dired-mark-omitted
      ;; "zc"  'dired-change-marks
      ;; "zs" 'dired-mark-subdir-files
      "s"  'dired-mark
      "u"  'dired-unmark
      "U"  'dired-unmark-all-marks
      "zd" 'dired-hide-details-mode
      "zt" 'dired-toggle-marks
      ;; (kbd "* <delete>") 'dired-unmark-backward
      ;; (kbd "* C-n") 'dired-next-marked-file
      ;; (kbd "* C-p") 'dired-prev-marked-file
      ;; Lower keys for commands not operating on all the marked files
      "d" 'dired-flag-file-deletion
      (kbd "C-m") 'dired-find-file
      "gr" 'revert-buffer
      "zww" 'dired-toggle-read-only
      ;; "I"   'dired-maybe-insert-subdir
      ;; "K"   'dired-do-kill-lines
      ;; "r" 'dired-do-redisplay
      "go" 'browse-url-of-dired-file
      "x" 'dired-do-flagged-delete
      "ga" 'dired-show-file-type ;; FIXME: This could probably go on a better key.
      "gf" 'find-file
      "gn" 'my-dired-create-file
      "gN" 'my-dired-create-directory
      "Y"  'dired-copy-filename-as-kill
      ;;; open
      "o" (lambda () (interactive) (dired-subtree-toggle) (dired-revert))
      (kbd "<return>") 'dired-find-file
      (kbd "S-<return>") 'dired-find-file-other-window
      ;;; Like preview
      (kbd "M-<return>") 'dired-display-file
      ;; "gO" 'dired-find-file-other-window
      ;; "go" 'dired-view-file
      ;; sort
      ;; "o" 'dired-sort-toggle-or-edit
      ;; moving
      ;; "gj" 'dired-next-dirline
      ;; "gk" 'dired-prev-dirline
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

(use-package dired-subtree)
(use-package dired-collapse)

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

(defun all-the-icons-dired-mode-hook()
  "Hook to setup all-the-icons."
  (dired-hide-details-mode 1)
  (dired-collapse-mode 1)
  (add-hook 'dired-after-readin-hook 'all-the-icons-dired--display t t)
  (all-the-icons-dired--display)
  )
(advice-add 'dired-revert :before #'all-the-icons-dired--reset)
(add-hook 'dired-mode-hook #'all-the-icons-dired-mode-hook)


(provide 'dired-config)
;;; dired-config.el ends here
