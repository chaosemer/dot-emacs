;; Lisp ------------------------------------------------------------------------
(require 'slime)

(setq inferior-lisp-program "sbcl")
(slime-setup)

(hook-minor-mode lisp-mode-hook
  hrule-mode
  (flyspell-prog-mode))