;;;; Text mode customizations.  -*- lexical-binding: t; -*-
;;;;
;;;; Why not use Emacs for editting plain old text?
(hook-mode text-mode-hook
  (unless (member major-mode '(mail-mode org-mode))
	visual-line-mode))

(hook-mode markdown-mode-hook
  ;; Normally, electric pair mode is on and helpful, but in Markdown
  ;; mode it makes URLs be automatically hidden before they're typed
  ;; in.
  (electric-pair-local-mode -1)

  (setf markdown-hide-markup t
        ;; So URLs hide automatically as they are typed.
        markdown-hide-urls t))
