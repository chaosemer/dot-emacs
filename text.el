;;;; Text mode customizations.
;;;;
;;;; Why not use Emacs for editting plain old text?
(require 'longlines)

(hook-mode text-mode-hook
  (unless (member major-mode '(mail-mode org-mode))
	longlines-mode))
