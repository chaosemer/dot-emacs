;; Emacs Lisp ------------------------------------------------------------------
(push (lambda () (eq major-mode 'emacs-lisp-mode)) semantic-inhibit-functions)
(hook-minor-mode emacs-lisp-mode-hook
  (setf (local-key-binding (kbd "M-.")) 'find-thing))

(defun read-symbol-name (prompt &optional default-value)
  "Read a symbol name, defaulting to DEFAULT-VALUE"
  (let ((read (completing-read prompt obarray)))
    (if (zerop (length read))
        default-value
      read)))

(defun* find-symbol (string &optional (obarray obarray))
  "Locate a symbol whose name is STRING in OBARRAY.

If a symbol named STRING is found in package, th symbol found is
returned as the first value; the second value is t.  Othrewise
returns `(values nil nil)'"
  (do-symbols (symbol obarray)
    (when (string= string (symbol-name symbol))
      (return-from find-symbol (values symbol t))))
  (values nil nil))

(defun find-thing (symbol)
  "Read and return an interned symbol, defaulting to the one near point."
  (interactive (let ((default (symbol-at-point)))
                 (list (if default
                           (read-symbol-name (format "Find symbol (default %s): " (symbol-name default)) (symbol-name default))
                         (read-symbol-name "Find symbol: ")))))

  (when (stringp symbol)
    (multiple-value-bind (interned accesable?) (find-symbol symbol)
      (setf symbol (and accesable? interned))))

  (find-function-do-it symbol
                       (cond ((facep symbol) 'defface)
                             ((fboundp symbol) nil)
                             ((and symbol (boundp symbol)) 'defvar)
                             (t (error "`%s' is not a face, function, or variable." symbol)))
                       'switch-to-buffer))