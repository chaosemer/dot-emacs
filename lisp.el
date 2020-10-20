;;;; (Common) Lisp customizations  -*- lexical-binding: t; -*-
(require 'hideif)
(require 'slime)

(setf inferior-lisp-program "sbcl")
(slime-setup)
(cl-pushnew '("\\.asd\\'" . lisp-mode) auto-mode-alist :test #'equal)

(hook-mode lisp-mode-hook
  (make-variable-buffer-local 'browse-url-browser-function)
  (setf browse-url-browser-function 'w3m)
  (font-lock-add-keywords nil '(("^\\s *;;;.*\n?" (0 'section-comment-face t))
                                ("^;;;;.*\n?" (0 'file-comment-face t))))
  ;;; Key bindings
  (setf (local-key-binding (kbd "C-<down-mouse-3>")) (lambda () (interactive)
                                                       (popup-menu slime-easy-menu))))

(define-key lisp-mode-map (kbd "TAB") nil)
