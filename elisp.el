;; Emacs Lisp ------------------------------------------------------------------
(defalias 'elisp-repl #'ielm)

(hook-minor-mode emacs-lisp-mode-hook
  hrule-mode)
