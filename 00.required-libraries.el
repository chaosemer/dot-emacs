;; Readability macros ----------------------------------------------------------
(require 'cl)

(defmacro hook-minor-mode (hook mode)
  `(add-hook ',hook (lambda () (,mode 1))))

(defun require-noerror (feature &optional filename)
  (require feature filename t))
