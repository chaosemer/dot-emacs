;; Readability macros ----------------------------------------------------------
(require 'cl)

(defmacro defmacro+ (name args &rest body)
  "Like `defmacro*', but also attempts to figure out the indenting"
  (let ((body-index (position-if (lambda (arg) (member arg '(&body &rest)))
                                 (remove '&optional args))))
    `(progn
       (setf (get ',name 'lisp-indent-function) ,body-index)
       (defmacro* ,name ,args ,@body))))

(defmacro+ hook-minor-mode (hook &body modes)
  "Hook each minor mode MODE on HOOK."
  (let ((accum '()))
    (dolist (mode modes)
      (push  `(add-hook ',hook (lambda () ,(if (listp mode) mode
                                             `(,mode 1))))
            accum))
    `(progn ,@(nreverse accum) ',hook)))

(defsetf lookup-key define-key)

(defun require-noerror (feature &optional filename)
  (require feature filename t))

(unless (fboundp 'cua-mode)
  (require 'cua)
  (message "Using old compatibility mode for CUA-MODE")
  (defalias 'cua-mode 'CUA-mode))
(unless (fboundp 'grep-tree)
  (message "Using old compatibility mode for GREP-TREE")
  (defalias 'grep-tree 'grep-find))
(unless (fboundp 'semantic-default-elisp-setup)
  (message "Using non-existant function SEMANTIC-DEFAULT-ELISP-SETUP")
  (defun semantic-default-elisp-setup ()))
