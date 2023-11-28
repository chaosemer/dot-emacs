;;; init/elisp.el --- Emacs Lisp customizations  -*- lexical-binding: t; -*-

;;; Code:
(hook-mode emacs-lisp-mode-hook
  eldoc-mode)
(hook-mode ielm-mode-hook
  eldoc-mode)

;;; Faces:
(font-lock-add-keywords 'emacs-lisp-mode
                        '(("^\\s *;;;.*\n?" (0 'section-comment-face t))
                          ("^;;;;.*\n?" (0 'file-comment-face t))))

;;; Keymaps:

;; emacs-lisp-mode defines the tab key in its map.  It shouldn't TODO(upstream)
(keymap-unset emacs-lisp-mode-map "TAB")
