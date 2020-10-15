;;;; Misc utilities that I like to use throughout my init file.  -*- lexical-binding: t; -*-
(require 'cl)
(require 'package)

;;; A new and improved DEFMACRO.
(defmacro defmacro+ (name args &rest body)
  "Like `defmacro*', but also attempts to figure out the indenting."
  (let ((body-index (position-if (lambda (arg) (member arg '(&body &rest)))
                                 (remove '&optional args))))
    `(progn
       (setf (get ',name 'lisp-indent-function) ,body-index)
       (defmacro* ,name ,args ,@body))))
(setf find-function-regexp
      "^\\s-*(\\(def\\(ine-skeleton\\|ine-generic-mode\\|ine-derived-mode\\|ine\\(?:-global\\)?-minor-mode\\|ine-compilation-mode\\|un-cvs-mode\\|foo\\|[^cfgv]\\w+\\*?\\+?\\)\\|easy-mmode-define-[a-z-]+\\|easy-menu-define\\|menu-bar-make-toggle\\)\\(?:\\s-\\|\n\\|;.*\n\\)+\\('\\|(quote \\)?%s\\(\\s-\\|$\\|(\\|)\\)")
(font-lock-add-keywords 'emacs-lisp-mode
                        '(("(\\(defmacro\\+\\) \\s *\\(\\(?:\\sw\\|\\s_\\)*\\)"
                           (1 'font-lock-keyword-face)
                           (2 'font-lock-function-name-face))))

;;; Allow me to declaratively hook things, automatically removing the hook when redefined.
(defmacro+ hook-mode (hook &body modes)
  "Hook each member of MODES on HOOK.  If the member is a symbol,
call (member 1); if it is a list, just execute it directly.

This allows you to declaratively hook in minor modes on a major mode."
  (let ((body (loop for expr in modes
                    if (and (symbolp expr) (fboundp expr))
                      collect (list expr 1)
                    else if (listp expr)
                      collect expr
                    else do (error "%s does not appear to name a minor mode." expr))))
    `(hook-mode-attach ',hook (lambda () "Auto-generated by `hook-mode'"
                                      ,@body))))

(defvar hook-mode-*hooks* (make-hash-table :test #'eq))
(defun hook-mode-attach (hook function)
  (when (gethash hook hook-mode-*hooks*)
    (remove-hook hook (gethash hook hook-mode-*hooks*)))
  (add-hook hook function)
  (setf (gethash hook hook-mode-*hooks*) function))

;;; Convenient shorthand for requiring without an error.
(defun require-noerror (feature &optional filename)
  "Like `require', but will return NIL on error."
  (require feature filename t))
