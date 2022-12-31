;;; evil-collection.el --- A set of keybindings for Evil mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'evil)

(defcustom evil-collection-key-whitelist '()
  "List of keys that may be used by Evil Collection.
This is a list of strings that are suitable for input to
`kbd'.  If there are no keys in the list, the whitelist will be ignored."
  :type '(repeat string)
  :group 'evil-collection)

(defcustom evil-collection-key-blacklist '()
  "List of keys that may not be used by Evil Collection.
This is a list of strings that are suitable for input to `kbd'."
  :type '(repeat string)
  :group 'evil-collection)

(defcustom evil-collection-state-passlist '()
  "List of evil states that may be used by Evil Collection.
This is a list of symbols that are suitable for input to
 `evil-define-key'. Ignore when there are no states in the list."
  :type '(repeat symbol)
  :group 'evil-collection)

(defun evil-collection--filter-states (state)
  "Return a list states after filtering STATE (a single symbol or list of symbols).
The return value adheres to `evil-collection-state-passlist' and
`evil-collection-state-denylist'. When the STATE is `nil', which
means all states for `evil-define-key', return `nil'."
  (let ((states (if (listp state) state (list state))))
    (seq-difference
     (if evil-collection-state-passlist
         (seq-intersection states evil-collection-state-passlist)
       states)
     evil-collection-state-denylist)))

(defcustom evil-collection-state-denylist
  (if (bound-and-true-p evil-disable-insert-state-bindings)
      '(insert)
    '())
  "List of evil states that may not be used by Evil Collection.
This is a list of symbols that are suitable for input to
 `evil-define-key'."
  :type '(repeat symbol)
  :group 'evil-collection)

(defun evil-collection--define-key (state map-sym bindings)
  "Workhorse function for `evil-collection-define-key'. See
`evil-collection-define-key' docstring for more details."
  (cond ((null bindings))
        ((and (boundp map-sym) (keymapp (symbol-value map-sym)))
         (condition-case-unless-debug err
             (apply #'evil-define-key*
                    state (symbol-value map-sym) bindings)
           (error
            (message "evil-collection: error setting key in %s %S"
                     map-sym err))))
        ((boundp map-sym)
         (user-error "evil-collection: %s is not a keymap" map-sym))
        (t
         (let* ((fname (format "evil-collection-define-key-in-%s" map-sym))
                (fun (make-symbol fname)))
           (fset fun `(lambda (&rest args)
                        (when (and (boundp ',map-sym) (keymapp ,map-sym))
                          (remove-hook 'after-load-functions #',fun)
                          (condition-case-unless-debug err
                              (apply #'evil-define-key*
                                     ',state ,map-sym ',bindings)
                            (error
                             (message
                              ,(format
                                "evil-collection: error setting key in %s %%S"
                                map-sym)
                              err))))))
           (add-hook 'after-load-functions fun t)))))


(defun evil-collection-define-key (state map-sym &rest bindings)
  "Wrapper for `evil-define-key*' with additional features.
Unlike `evil-define-key*' MAP-SYM should be a quoted keymap other than the
unquoted keymap required for `evil-define-key*'. This function adds the ability
to filter keys on the basis of `evil-collection-key-whitelist' and
`evil-collection-key-blacklist'. It also records bindings with annalist.el."
  (declare (indent defun))
  (let* ((whitelist (mapcar 'kbd evil-collection-key-whitelist))
         (blacklist (mapcar 'kbd evil-collection-key-blacklist))
         (states-to-bind (evil-collection--filter-states state))
         filtered-bindings)
    (when (or states-to-bind (null state))
      (while bindings
        (let ((key (pop bindings))
              (def (pop bindings)))
          (when (or (and whitelist (member key whitelist))
                    (not (member key blacklist)))
            (push key filtered-bindings)
            (push def filtered-bindings))))
      (setq filtered-bindings (nreverse filtered-bindings))
      (evil-collection--define-key states-to-bind map-sym filtered-bindings))))

(provide 'my-evil-collection-minimal)
;;; my-evil-collection-minimal.el ends here
