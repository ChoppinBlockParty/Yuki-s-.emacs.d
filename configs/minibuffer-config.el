
;;; evil-collection-minibuffer.el --- Evil bindings for the minibuffer -*- lexical-binding: t -*-

;;; Commentary:
;; Evil bindings for the minibuffer.

;;; Code:
(defconst evil-collection-minibuffer-maps '(minibuffer-local-map
                                            minibuffer-local-ns-map
                                            minibuffer-local-completion-map
                                            minibuffer-local-must-match-map
                                            minibuffer-local-isearch-map
                                            evil-ex-completion-map))

(defun evil-collection-minibuffer-insert ()
  "Switch to insert state.

This function is meant to be hooked in the minibuffer:

  (add-hook 'minibuffer-setup-hook 'evil-collection-minibuffer-insert)

`evil-set-initial-state' can not be used for the minibuffer since
it does not have a mode."
  (set (make-local-variable 'evil-echo-state) nil)
  ;; (evil-set-initial-state 'mode 'insert) is the evil-proper
  ;; way to do this, but the minibuffer doesn't have a mode.
  ;; The alternative is to create a minibuffer mode (here), but
  ;; then it may conflict with other packages' if they do the same.
  (evil-insert 1))

(defun evil-collection-minibuffer-setup ()
  "Initialize minibuffer for `evil'."
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Text-from-Minibuffer.html
  (dolist (map '(minibuffer-local-map
                 minibuffer-local-ns-map
                 minibuffer-local-completion-map
                 minibuffer-local-must-match-map
                 minibuffer-local-isearch-map))
    (evil-define-key 'normal map (kbd "<escape>") 'abort-recursive-edit)
    (evil-define-key 'normal map (kbd "<return>") 'exit-minibuffer)
    )

  (add-hook 'minibuffer-setup-hook 'evil-collection-minibuffer-insert)
  ;; Because of the above minibuffer-setup-hook, some evil-ex bindings need be reset.
  (evil-define-key 'normal 'evil-ex-completion-map (kbd "<escape>") 'abort-recursive-edit)
  (evil-define-key 'insert 'evil-ex-completion-map (kbd "C-p") 'previous-complete-history-element)
  (evil-define-key 'insert 'evil-ex-completion-map (kbd "C-n") 'next-complete-history-element)
  (evil-define-key 'normal 'evil-ex-completion-map (kbd "C-p") 'previous-history-element)
  (evil-define-key 'normal 'evil-ex-completion-map (kbd "C-n") 'next-history-element))

;;;
;; (evil-collection-minibuffer-setup)

(provide 'minibuffer-config)
