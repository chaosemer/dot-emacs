;;;; Patches for buggy/old emacs code
;;;;
;;;; This allows me to use one init file across multiple versions of Emacs.
;;;;
;;;; Currently tested against:
;;;; * GNU Emacs 26.1
;;;; * GNU Emacs 27.1
(require 'log-edit)
(require 'compile)

(progn
  (display-warning 'emacs "Adding setf expansions for missing things")
  (defsetf lookup-key define-key))

;; Disallow navigating to the minibuffer
(unless (eq (plist-get minibuffer-prompt-properties 'cursor-intangible) t)
  (display-warning 'emacs "Disallowing navigation into the minibuffer prompt")
  (setf minibuffer-prompt-properties
	(plist-put minibuffer-prompt-properties 'cursor-intangible t))
  (add-hook 'minibuffer-setup-hook 'cursor-intangible-mode))

;; Fix buggy regexp in Emacs
(let ((correct-regexp "^ *\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) ?: \\(?:see declaration\\|\\(?:warnin\\(g\\)\\|[a-z ]+\\) [A-Z][0-9]+:\\)"))
  (unless (equal (second (assoc 'msft compilation-error-regexp-alist-alist)) correct-regexp)
    (display-warning 'emacs "Fixing buggy Microsoft regexp")
    (setf (second (assoc 'msft compilation-error-regexp-alist-alist)) correct-regexp)))

;; Making C-a in log-edit-mode not be there
(when (lookup-key log-edit-mode-map (kbd "C-a"))
  (display-warning 'emacs "Cleaning up log-edit-mode-map")
  (setf (lookup-key log-edit-mode-map (kbd "<remap> <beginning-of-line>"))
		(lookup-key log-edit-mode-map (kbd "C-a")))
  (setf (lookup-key log-edit-mode-map (kbd "C-a")) nil))
