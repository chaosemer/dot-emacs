;;; init/elisp.el --- Emacs Lisp customizations  -*- lexical-binding: t; -*-

;;; Code:
(add-hook 'emacs-lisp-mode-hook
          (defun my-emacs-lisp-mode-hook ()
            (eldoc-mode)))
(defvar ielm-mode-hook)
(add-hook 'ielm-mode-hook
          (defun my-ielm-mode-hook ()
            (eldoc-mode)))

;;; Faces:
(font-lock-add-keywords 'emacs-lisp-mode
                        '(("^\\s *;;;.*\n?" (0 'section-comment-face t))
                          ("^;;;;.*\n?" (0 'file-comment-face t))))
