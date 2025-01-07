;;; init/lisp.el --- (Common) Lisp customizations  -*- lexical-binding: t; -*-

;;; Code:
(add-hook 'lisp-mode-hook
          (defun my-lisp-mode-hook ()
            (make-variable-buffer-local 'browse-url-browser-function)
            (setf browse-url-browser-function 'w3m)
            (font-lock-add-keywords nil '(("^\\s *;;;.*\n?" (0 'section-comment-face t))
                                          ("^;;;;.*\n?" (0 'file-comment-face t))))

            (require 'slime)
            (setf inferior-lisp-program "sbcl")
            (slime-setup)))

;;; Keymaps:
(keymap-set lisp-mode-map "C-<down-mouse-3>" (lambda () (interactive)
                                               (popup-menu slime-easy-menu)))
(keymap-unset lisp-mode-map "TAB")
