;;;; Emacs Lisp customizations
(require 'find-func)

(hook-mode emacs-lisp-mode-hook
  eldoc-mode
  (setf (local-key-binding (kbd "M-.")) 'find-thing
        (local-key-binding (kbd "C-x 4 .")) 'find-thing-other-window
        (local-key-binding (kbd "C-x 5 .")) 'find-thing-other-frame)
  (font-lock-add-keywords nil '(("^\\s *;;;.*\n?" (0 'section-comment-face t))
                                ("^;;;;.*\n?" (0 'file-comment-face t)))))
(hook-mode ielm-mode-hook
  eldoc-mode)

;; NOTE:
;; this function should be added to cl.el
(defun* find-symbol (string &optional (obarray obarray))
  "Locate a symbol whose name is STRING in OBARRAY.

If a symbol named STRING is found in package, the symbol found is
returned as the first value; the second value is t.  Othrewise
returns `(values nil nil)'"
  (do-symbols (symbol obarray)
    (when (string= string (symbol-name symbol))
      (return-from find-symbol (values symbol t))))
  (values nil nil))

(defun find-thing (symbol)
  "Find the definition of the object named by SYMBOL.

Searches for a face, then a function, finally a variable.  Use
`find-function', `find-variable', or `find-face-definition' if
you know the exact type you want."
  (interactive (list (find-thing-read "Find symbol")))

  (find-thing-do-it symbol 'switch-to-buffer))

(defun find-thing-other-window (symbol)
  "Find, in another window, the definition of the object named by SYMBOL.

See `find-thing'."
  (interactive (list (find-thing-read "Find symbol in other window")))
  (find-thing-do-it symbol 'switch-to-buffer-other-window))

(defun find-thing-other-frame (symbol)
  "Find, in another frame, the definition of the object named by SYMBOL.

See `find-thing'."
  (interactive (list (find-thing-read "Find symbol in other window")))
  (find-thing-do-it symbol 'switch-to-buffer-other-window))

(defun read-symbol-name (prompt &optional default-value)
  "Read a symbol name, defaulting to DEFAULT-VALUE"
  (let ((read (completing-read prompt obarray)))
    (if (zerop (length read))
        default-value
      read)))

(defun find-thing-read (prompt-header)
  "Read an interned symbol, returns NIL if nothing is specfied." 
  (let* ((default (symbol-at-point))
         (symbol-name (if default
                          (read-symbol-name (format "%s (default %s): "
                                                    prompt-header
                                                    (symbol-name default))
                                            default)
                        (read-symbol-name (format "%s: " prompt-header)))))
    (multiple-value-bind (interned accesable?) (find-symbol symbol-name)
      (and accesable? interned))))

(defun find-thing-do-it (symbol switch-fn)
  (find-function-do-it symbol
                       (cond ((facep symbol) 'defface)
                             ((fboundp symbol) nil)
                             ((and symbol (boundp symbol)) 'defvar)
                             (t (error "`%s' is not a face, function, or variable" symbol)))
                       switch-fn))

;; emacs-lisp-mode defines the tab key in its map.  It shouldn't
(define-key emacs-lisp-mode-map (kbd "TAB") nil)
