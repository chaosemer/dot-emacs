;; Lisp ------------------------------------------------------------------------
(require 'slime)

(setq inferior-lisp-program "sbcl")
(slime-setup)
(add-to-list 'lisp-mode-hook 'flyspell-prog-mode) ; flyspell-prog-mode isn't like other minor modes