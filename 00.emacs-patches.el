;;;; Patches for buggy/old emacs code
;;;;
;;;; This allows me to use one init file across multiple versions of Emacs.
;;;;
;;;; Currently tested against:
;;;; * GNU Emacs 25.3
;;;; * GNU Emacs 26.1
(require 'log-edit)
(require 'compile)

(progn
  (display-warning 'emacs "Adding setf expansions for missing things")
  (defsetf lookup-key define-key))

(progn
  (display-warning 'emacs "Making git annotate faster")
  (require 'vc-git)
  (defun vc-git-annotate-command (file buf &optional rev)
	(let ((name (file-relative-name file)))
	  (vc-git-command buf 'async nil "blame" "--date=iso" rev "--" name))))

(progn
  (display-warning 'emacs "Cleaning up file menu")
  (define-key menu-bar-file-menu [new-file]
    '(menu-item "New File"
                (lambda ()
                  (interactive)
                  (switch-to-buffer (generate-new-buffer "untitled")))
                :enable (menu-bar-non-minibuffer-window-p)
                :help "Create a new buffer"))
  (define-key menu-bar-file-menu [open-file]
    '(menu-item "Open File..." find-file
                :enable (menu-bar-non-minibuffer-window-p)
                :help "Open an existing file"))
  (define-key menu-bar-file-menu [print-buffer] nil)
  (define-key menu-bar-file-menu [print-region] nil)
  (define-key menu-bar-file-menu [ps-print-buffer-faces] nil)
  (define-key menu-bar-file-menu [ps-print-region-faces] nil)
  (define-key menu-bar-file-menu [ps-print-buffer] nil)
  (define-key menu-bar-file-menu [ps-print-region] nil))

;; Disallow navigating to the minibuffer
(unless (eq (plist-get minibuffer-prompt-properties 'point-entered) 'minibuffer-avoid-prompt)
  (display-warning 'emacs "Disallowing navigation into the minibuffer prompt")
  (setf minibuffer-prompt-properties
		(plist-put minibuffer-prompt-properties 'point-entered 'minibuffer-avoid-prompt)))

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

;; Make the tty face look better
(defface my-menu
  '((t :inherit mode-line))
  "My replacement for the face `menu'.")
(unless (face-equal 'my-menu 'menu)
  (display-warning 'emacs "Making the menu face look better.")
  (copy-face 'my-menu 'menu))

;(setf inhibit-frame-set-background-mode t)
;; (require 'term/xterm)
;; (display-warning 'emacs (format "%s" (face-background 'show-paren-match)))
;; (terminal-init-xterm)
;; (display-warning 'emacs (format "%s" (face-background 'show-paren-match)))
