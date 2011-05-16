;;;; Patches for buggy/old emacs code
;;;;
;;;; This allows me to use one init file across multiple versions of Emacs.
;;;;
;;;; Currently tested against:
;;;; * GNU Emacs 22.3

;;; Used in GNU Emacs 23.2
(progn
  (display-warning 'emacs "Adding setf expansion for `define-key'")
  (defsetf lookup-key define-key))

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

;;; Used in GNU Emacs 22.1
;; I might not have Semantic installed, because it is uninstallable on Debian.  Temp fix:
(unless (require 'semantic nil t)
  (display-warning 'semantic "Semantic is NOT installed.  Creating stub functions...")
  (defun global-semanticdb-minor-mode (&rest args))
  (defun global-semantic-idle-scheduler-mode (&rest args))
  (defun global-semantic-show-parser-state-mode (&rest args))
  (defun global-semantic-idle-summary-mode (&rest args))
  (defvar semantic-inhibit-functions nil)
  (defvar semanticdb-project-roots nil))

(unless (fboundp 'warn)
  (message "Using old compatibility mode for `warn'")
  (defalias 'warn 'message))

(unless (fboundp 'next-error-follow-minor-mode)
  (display-warning 'emacs "Using old compatibility mode for `next-error-follow-minor-mode")
  (defun next-error-follow-minor-mode (arg)))

(unless (fboundp 'cua-mode)
  (require 'cua)
  (display-warning 'emacs "Using old compatibility mode for `cua-mode'")
  (defalias 'cua-mode 'CUA-mode))

(unless (fboundp 'rgrep)
  (display-warning 'emacs "Using old compatibility mode for `rgrep'")
  (defalias 'rgrep (if (fboundp 'grep-tree) 'grep-tree 'grep-find)))

(unless (fboundp 'global-semantic-idle-completions-mode)
  (display-warning 'semantic "Using old compatibility mode for `global-semantic-idle-completions-mode'")
  (defalias 'global-semantic-idle-completions-mode 'global-semantic-idle-scheduler-mode))

(unless (fboundp 'x-show-tip)
  (display-warning 'emacs "Modifying `tooltip-mode' to do nothing or error.")
  (defun tooltip-mode (&optional arg)
    (interactive (list (prefix-numeric-value current-prefix-arg)))
    (when (> arg 0)
      (error "Sorry, tooltips are not yet available on this system"))))

(unless (fboundp 'custom-autoload)
  (display-warning 'emacs "Using old compatibility mode for `custom-autoload'")
  (defalias 'custom-autoload 'custom-add-load))

(let ((move-fns '(c-forward-conditional c-backward-conditional
                  c-down-conditional c-up-conditional
                  c-down-conditional-with-else
                  c-up-conditional-with-else
                  c-beginning-of-statement c-end-of-statement)))
  (require 'cua-base)
  (dolist (symbol move-fns)
    (unless (eq 'move (get symbol 'CUA))
      (display-warning 'emacs (format "Adding CUA property to `%s'." symbol))
      (setf (get symbol 'CUA) 'move))))

(unless (fboundp 'global-hi-lock-mode)
  (display-warning 'emacs "Using old compatibility mode for GLOBAL-HI-LOCK-MODE")
  (defalias 'global-hi-lock-mode 'hi-lock-mode))

(when (require 'tramp nil t)
  (unless (string< "2.0.52" tramp-version)
    (require 'tramp-smb)
    (display-warning 'emacs "Disabling backups on remote files.  TRAMP has issues with smb backups files.")
    (defun tramp-smb-wait-for-output (user host)
      "Wait for output from smbclient command.
Returns nil if an error message has appeared."
      (let ((proc (get-buffer-process (current-buffer)))
	    (found (progn (goto-char (point-min))
			  (re-search-forward tramp-smb-prompt nil t)))
	    (err   (progn (goto-char (point-min))
			  (re-search-forward tramp-smb-errors nil t))))

	;; Algorithm: get waiting output.  See if last line contains
	;; tramp-smb-prompt sentinel or tramp-smb-errors strings.
	;; If not, wait a bit and again get waiting output.
	(while (and (not found) (not err))

	  ;; Accept pending output.
	  (tramp-accept-process-output proc)

	  ;; Search for prompt.
	  (goto-char (point-min))
	  (setq found (re-search-forward tramp-smb-prompt nil t))

	  ;; Search for errors.
	  (goto-char (point-min))
	  (setq err (re-search-forward tramp-smb-errors nil t)))

	;; Add output to debug buffer if appropriate.
	(when tramp-debug-buffer
	  (append-to-buffer
	   (tramp-get-debug-buffer nil tramp-smb-method user host)
	   (point-min) (point-max)))

	;; Return value is whether no error message has appeared.
	(not err)))))

(unless (fboundp 'define-global-minor-mode)
  (display-warning 'emacs "Using old compatibiltily mode for `define-global-minor-mode'.")
  (defalias 'define-global-minor-mode 'easy-mmode-define-global-mode))


(when (and (fboundp 'c-subword-mode) (not (fboundp 'global-c-subword-mode)))
  (display-warning 'emacs "Defining global-c-subword-mode")
  (define-global-minor-mode global-c-subword-mode c-subword-mode
    (lambda () (c-subword-mode 1))))

(unless (fboundp 'global-subword-mode)
  (defalias global-subword-mode global-c-subword-mode))

;; Fix buggy regexp in Emacs
(let ((correct-regexp "^ *\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) ?: \\(?:see declaration\\|\\(?:warnin\\(g\\)\\|[a-z ]+\\) [A-Z][0-9]+:\\)"))
  (unless (equal (second (assoc 'msft compilation-error-regexp-alist-alist)) correct-regexp)
    (display-warning 'emacs "Fixing buggy Microsoft regexp")
    (setf (second (assoc 'msft compilation-error-regexp-alist-alist)) correct-regexp)))
