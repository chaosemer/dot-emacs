;;;; -*- Mode: emacs-lisp; lexical-binding: t; -*-
;;;; MJF Emacs customizations
;;;; TODO(upstream/package)

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

(defvar *init-file-directory* (expand-file-name "init" user-emacs-directory)
  "Directory where user initialization files are.

At Emacs initialization, all files in this directory will be
loaded (via `load') in alphabetical order.  Setting this
variable after Emacs startup has no effect.")

;;; Helper functions and required functionality
(defun file-init-loadable? (file)
  "Tests if FILE should be loaded at Emacs initialization."
  (and (file-regular-p file)
       (member (file-name-extension file t) load-suffixes)
       (not (member (file-name-nondirectory file) '("init.el" "early-init.el")))))

(defun directory-files-filter (directory predicate &optional full match nosort)
  "Return a list of names of files in DIRECTORY, excluding any files that don't satisfy PREDICATE"
  (let ((files '()))
    (dolist (file (directory-files directory full match nosort))
      (when (funcall predicate file)
        (push file files)))
    (nreverse files)))

(defvar init--long-load-time-warning 0.05
  "Controls if a particular file gets a warning if it takes too long to load.

Best practice is to increment this using `cl-incf' next to known
slow operations.  This can also be set to `nil' to completely
disable the long load warning.")

;; add the directories in *LOCAL-LOAD-PATH* to LOAD-PATH
(when (file-exists-p *local-load-path*)
(let ((save-abbrevs nil))
  (byte-recompile-directory *local-load-path*))
(add-to-list 'load-path *local-load-path*)
(dolist (file (directory-files *local-load-path*))
  (let ((full-path (expand-file-name file *local-load-path*)))
    (when (and (file-directory-p full-path)
               (not (member file '("." ".."))))
      (add-to-list 'load-path full-path)))))

; load each FILE-INIT-LOADABLE? file in *INIT-FILE-DIRECTORY* once
(let ((prev-time (time-convert nil 'list))
      (timing-messages '()))
  (dolist (file (delete-dups
                 (mapcar #'file-name-sans-extension
                         (directory-files-filter *init-file-directory*
                                                 #'file-init-loadable?
                                                 t))))
    (let ((debug-ignored-errors '())
          (debug-on-error t)
          (debug-on-quit t)
          (init--long-load-time-warning init--long-load-time-warning))
      (load file)
      (let* ((cur-time (time-convert nil 'list))
             (delta-time (float-time (time-subtract cur-time prev-time))))
        (when (and init--long-load-time-warning (> delta-time init--long-load-time-warning))
          (push (format "Loading `%s' took %f seconds."
                        file delta-time)
                timing-messages))
        (setf prev-time cur-time))))

  ;; Helpful debugging for init files.
  (when timing-messages
    (dolist (message (nreverse timing-messages))
      (display-warning 'init message))))
