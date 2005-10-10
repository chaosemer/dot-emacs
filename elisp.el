;; Emacs Lisp ------------------------------------------------------------------
(push (lambda () (eq major-mode 'emacs-lisp-mode)) semantic-inhibit-functions)
(hook-minor-mode emacs-lisp-mode-hook
  (setf (local-key-binding (kbd "M-.")) 'find-thing))

(defun find-thing (symbol)
  "Read and return an interned symbol, defaulting to the one near point."
  (interactive (list (symbol-at-point)))

  (find-function-do-it symbol
                       (cond ((facep symbol) 'defface)
                             ((fboundp symbol) nil)
                             ((and symbol (boundp symbol)) 'defvar)
                             (t (error "`%s' is not a face, function, or variable." symbol)))
                       'switch-to-buffer))