;; Readability macros ----------------------------------------------------------
(require 'cl)

(defmacro hook-minor-mode (hook mode)
  `(add-hook ',hook (lambda () (,mode 1))))

(defun require-noerror (feature &optional filename)
  (require feature filename t))

(unless (fboundp 'cua-mode)
  (require 'cua)
  (message "Using old compatibility mode for CUA-MODE")
  (defalias 'cua-mode 'CUA-mode))
(unless (fboundp 'grep-tree)
  (message "Using old compatibility mode for GREP-TREE")
  (defalias 'grep-tree 'grep-find))
