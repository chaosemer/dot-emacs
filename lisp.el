;; Lisp ------------------------------------------------------------------------
(require 'slime)

(setf inferior-lisp-program "sbcl")
(slime-setup)
(pushnew '("\\.asd\\'" . lisp-mode) auto-mode-alist :test #'equal)
(push (lambda () (eq major-mode 'lisp-mode)) semantic-inhibit-functions)

(hook-minor-mode lisp-mode-hook
  (setf browse-url-browser-function 'w3m)
    ;; Key bindings
  (setf (local-key-binding (kbd "C-<down-mouse-3>")) (lambda () (interactive)
                                                       (popup-menu slime-easy-menu)))
  (make-variable-buffer-local 'browse-url-browser-function))

