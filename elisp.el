;;; init/elisp.el --- Emacs Lisp customizations  -*- lexical-binding: t; -*-

;;; Declarations:
(defvar elisp-fontify-semantically)
(defvar elisp-add-help-echo)
(defvar ielm-dynamic-return)
(defvar ielm-mode-hook)

;;; Code:
(defun my-emacs-lisp-mode-hook ()
  (make-local-variable 'sentence-end-double-space)
  (setf sentence-end-double-space t))
(add-hook 'emacs-lisp-mode-hook #'my-emacs-lisp-mode-hook)
(add-hook 'ielm-mode-hook #'my-emacs-lisp-mode-hook)

(setf ielm-dynamic-return 'point
      elisp-fontify-semantically t
      ;; Adding help echo spams way too much data in the echo area.
      ;; For now, just silence it, maybe a future Emacs version makes
      ;; this usable.
      elisp-add-help-echo nil)

;;; Faces:
(font-lock-add-keywords 'emacs-lisp-mode
                        '(("^\\s *;;;.*\n?" (0 'section-comment-face t))
                          ("^;;;;.*\n?" (0 'file-comment-face t))))
