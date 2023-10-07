;;;; Emacs Lisp customizations  -*- lexical-binding: t; -*-
(hook-mode emacs-lisp-mode-hook
  eldoc-mode
  (font-lock-add-keywords nil '(("^\\s *;;;.*\n?" (0 'section-comment-face t))
                                ("^;;;;.*\n?" (0 'file-comment-face t)))))
(hook-mode ielm-mode-hook
  eldoc-mode)

;; emacs-lisp-mode defines the tab key in its map.  It shouldn't TODO(upstream)
(keymap-unset emacs-lisp-mode-map "TAB")
