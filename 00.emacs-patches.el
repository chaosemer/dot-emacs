;;;; Patches for buggy/old emacs code
;;;;
;;;; This allows me to use one init file across multiple versions of Emacs.
;;;;
;;;; Currently tested against:
;;;; * GNU Emacs 23.4

(progn
  (display-warning 'emacs "Adding setf expansion for `define-key'")
  (defsetf lookup-key define-key))

(progn
  (display-warning 'emacs "Loading legacy support for `vc-svn'")
  (require 'vc-svn17))

(progn
  (display-warning 'emacs "Making git annotate faster")
  (require 'vc-git)
  (defun vc-git-annotate-command (file buf &optional rev)
	(let ((name (file-relative-name file)))
	  (vc-git-command buf 'async nil "blame" "--date=iso" rev "--" name))))

(progn
  (display-warning 'emacs "Cleaning up file menu bar")
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

;; Fix buggy regexp in Emacs
(let ((correct-regexp "^ *\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) ?: \\(?:see declaration\\|\\(?:warnin\\(g\\)\\|[a-z ]+\\) [A-Z][0-9]+:\\)"))
  (unless (equal (second (assoc 'msft compilation-error-regexp-alist-alist)) correct-regexp)
    (display-warning 'emacs "Fixing buggy Microsoft regexp")
    (setf (second (assoc 'msft compilation-error-regexp-alist-alist)) correct-regexp)))

;; Fix subword mode
(progn
  (require 'subword)
  (unless (fboundp 'subword-left)
    (display-warning 'emacs "Defining `subword-left' and `subword-right'")
    (defun subword-left (&optional n)
      "Do the same as `left-word', but on subwords.
See the command `subword-mode' for a description of subwords.
Optional argumen ARG is the same as for `left-word'."
      (interactive "p")
      (if (eq (current-bidi-paragraph-direction) 'left-to-right)
          (subword-backward n)
        (subword-forward n)))
    (defun subword-right (&optional n)
      "Do the same as `right-word', but on subwords.
See the command `subword-mode' for a description of subwords.
Optional argumen ARG is the same as for `right-word'."
      (interactive "p")
      (subword-left (- (or n 1))))
    (put 'subword-left 'CUA 'move)
    (put 'subword-right 'CUA 'move)
    (define-key subword-mode-map (vector 'remap 'left-word) 'subword-left)
    (define-key subword-mode-map (vector 'remap 'right-word) 'subword-right)))
