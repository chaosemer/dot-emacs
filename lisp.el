;;; init/lisp.el --- (Common) Lisp customizations  -*- lexical-binding: t; -*-

;;; Declarations:
(declare-function slime-setup "slime")
(defvar inferior-lisp-program)
(defvar slime-easy-menu)

;;; Code:
(add-hook 'lisp-mode-hook
          (defun my-lisp-mode-hook ()
            (setq-local browse-url-browser-function 'w3m)
            (font-lock-add-keywords nil '(("^\\s *;;;.*\n?" (0 'section-comment-face t))
                                          ("^;;;;.*\n?" (0 'file-comment-face t))))

            (require 'slime)
            (setf inferior-lisp-program "sbcl")
            (slime-setup)))

;;; Keymaps:
(keymap-set lisp-mode-map "C-<down-mouse-3>" (lambda () (interactive)
                                               (popup-menu slime-easy-menu)))
(keymap-unset lisp-mode-map "TAB")
