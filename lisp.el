;; Lisp ------------------------------------------------------------------------
(require 'slime)

(setf inferior-lisp-program "sbcl")
(slime-setup)
(pushnew '("\\.asd\\'" . lisp-mode) auto-mode-alist :test #'equal)

(hook-minor-mode lisp-mode-hook
  hrule-mode
  ;(flyspell-prog-mode)

  ;; Key bindings
  (setf (local-key-binding (kbd "C-<down-mouse-3>")) (lambda () (interactive)
                                                       (popup-menu slime-easy-menu))))
(hook-minor-mode slime-temp-buffer-mode-hook
  hrule-mode)
