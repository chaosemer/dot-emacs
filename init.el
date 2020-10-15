;;;; -*- Mode: emacs-lisp; lexical-binding: t; -*-
;;;; MJF Emacs customizations

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;;; Variables to customize Emacs initialization.  Change these if you
;;; want to.

(defvar *local-load-path* (expand-file-name "~/local/lib/emacs/")
  "The root of all user local emacs libraries.

Files in this directory, and in subdirectories of this directory
will be added to the load path.")

(defvar *init-file-directory* (expand-file-name "~/.emacs.d/init/")
  "Directory where user initialization files are.

At Emacs initialization, all files in this directory will be
loaded (via `load') in alphabetical order.  Setting this
variable after Emacs startup has no effect.")

;;; Helper functions and required functionality
(require 'cl)

(defun file-init-loadable? (file)
  "Tests if FILE should be loaded at Emacs initialization."
  (and (file-regular-p file)
       (member (file-name-extension file t) load-suffixes)
       (not (string= (file-name-nondirectory file) "init.el"))))

(defun push-load-path! (directory)
  "Adds DIRECTORY to `load-path'"
  (add-to-list 'load-path directory))

(defun directory-files-filter (directory predicate &optional full match nosort)
  "Return a list of names of files in DIRECTORY, excluding any files that don't satisfy PREDICATE"
  (let ((files '()))
    (dolist (file (directory-files directory full match nosort))
      (when (funcall predicate file)
        (push file files)))
    (nreverse files)))

(unless (boundp 'load-suffixes)
  (defvar load-suffixes '(".elc" ".el")
    "This variable was added by version 21.3.50."))

;; add the directories in *LOCAL-LOAD-PATH* to LOAD-PATH
(when (file-exists-p *local-load-path*)
(let ((save-abbrevs nil))
  (byte-recompile-directory *local-load-path*))
(push-load-path! *local-load-path*)
(dolist (file (directory-files *local-load-path*))
  (let ((full-path (expand-file-name file *local-load-path*)))
    (when (and (file-directory-p full-path)
               (not (member file '("." ".."))))
      (push-load-path! full-path)))))

; load each FILE-INIT-LOADABLE? file in *INIT-FILE-DIRECTORY* once
(let ((debug-ignored-errors '())
      (debug-on-error t)
      (debug-on-quit t))
  (mapc #'load (remove-duplicates
		(mapcar #'file-name-sans-extension
			(directory-files-filter *init-file-directory*
						#'file-init-loadable?
						t))
		:test #'string=)))
