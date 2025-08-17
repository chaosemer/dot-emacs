;;; init/markdown.el --- Markdown customizations -*- lexical-binding: t; -*-

;;; Custom commands

;; Patch markdown mode
(with-eval-after-load 'markdown-mode
  (display-warning 'emacs "Removing - and + as bullets in Markdown")
  (defvar markdown-regex-list)
  (setf markdown-regex-list
	"^\\([[:blank:]]*\\)\\([#0-9]+\\.\\|[*:]\\)\\([[:blank:]]+\\)\\(\\[[ Xx]][[:blank:]]*\\)?"))
