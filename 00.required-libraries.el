;; Readability macros ----------------------------------------------------------
(require 'cl)

(defmacro hook-minor-mode (hook &rest modes)
  "Hook each minor mode MODE on HOOK."
  (let ((accum '()))
    (dolist (mode modes)
      (push  `(add-hook ',hook (lambda () ,(if (listp mode) mode
                                             `(,mode 1))))
            accum))
    `(progn ,@(nreverse accum) ',hook)))
(setf (get 'hook-minor-mode 'lisp-indent-function) 1)
    

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