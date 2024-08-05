;;; init/elisp.el --- Emacs Lisp customizations  -*- lexical-binding: t; -*-

;;; Code:
(hook-mode emacs-lisp-mode-hook
  eldoc-mode)
(defvar ielm-mode-hook)
(hook-mode ielm-mode-hook
  eldoc-mode)

;;; Faces:
(font-lock-add-keywords 'emacs-lisp-mode
                        '(("^\\s *;;;.*\n?" (0 'section-comment-face t))
                          ("^;;;;.*\n?" (0 'file-comment-face t))))
