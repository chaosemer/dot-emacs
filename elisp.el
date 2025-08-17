;;; init/elisp.el --- Emacs Lisp customizations  -*- lexical-binding: t; -*-

;;; Declarations:
(defvar ielm-mode-hook)

;;; Code:
(defun my-emacs-lisp-mode-hook ()
  (eldoc-mode)
  (make-local-variable 'sentence-end-double-space)
  (setf sentence-end-double-space t))
(add-hook 'emacs-lisp-mode-hook #'my-emacs-lisp-mode-hook)
(add-hook 'ielm-mode-hook #'my-emacs-lisp-mode-hook)

;;; Faces:
(font-lock-add-keywords 'emacs-lisp-mode
                        '(("^\\s *;;;.*\n?" (0 'section-comment-face t))
                          ("^;;;;.*\n?" (0 'file-comment-face t))))
