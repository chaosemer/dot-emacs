;;;; (Common) Lisp customizations
(require 'hideif)
(require 'slime)

(setf inferior-lisp-program "sbcl")
(slime-setup)
(pushnew '("\\.asd\\'" . lisp-mode) auto-mode-alist :test #'equal)
(push (lambda () (eq major-mode 'lisp-mode)) semantic-inhibit-functions)

(hook-mode lisp-mode-hook
  (make-variable-buffer-local 'browse-url-browser-function)
  (setf browse-url-browser-function 'w3m)
  (font-lock-add-keywords nil '(("^\\s *;;;.*$" (0 'section-comment-face t))
                                ("^;;;;.*$" (0 'file-comment-face t))))
  ;;; Key bindings
  (setf (local-key-binding (kbd "C-<down-mouse-3>")) (lambda () (interactive)
                                                       (popup-menu slime-easy-menu))))

