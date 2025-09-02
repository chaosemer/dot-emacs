;;; init/markdown.el --- Markdown customizations -*- lexical-binding: t; -*-

;;; Declarations:
(defvar markdown-fontify-code-blocks-natively)

;;; Code:

;; Actually, I prefer GitHub flavored markdown
(add-to-list 'major-mode-remap-alist '(markdown-mode . gfm-mode))

;; Make sure we delete selection on highlight.
(put 'markdown-enter-key 'delete-selection t)

(setf markdown-fontify-code-blocks-natively t)

;;; Faces:
(with-eval-after-load 'markdown-mode
  (set-face-attribute 'markdown-header-delimiter-face nil :height 0.7))
