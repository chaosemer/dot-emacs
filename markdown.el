;;; init/markdown.el --- Markdown customizations -*- lexical-binding: t; -*-

;;; Declarations:
(defvar markdown-fontify-code-blocks-natively)
(defvar markdown-hide-markup)
(defvar markdown-mode-map)

;;; Code:

;; Actually, I prefer GitHub flavored markdown
(add-to-list 'major-mode-remap-alist '(markdown-mode . gfm-mode))

;; Make sure we delete selection on highlight.
(put 'markdown-enter-key 'delete-selection t)

(setf markdown-fontify-code-blocks-natively t)

;; Always replace bullets with their cooler looking style
(defun always-prettify-markdown (old-fn last)
  "Advice function for `markdown-fontify-list-items' and others.

This advices:
* `markdown-fontity-blockquotes'
* `markdown-fontity-hrs'
* `markdown-fontify-list-items'
* `markdown-fontify-sub-superscripts'

OLD-FN: Old function, provided by advice system.
LAST: See adviced functions, above."
  (let ((markdown-hide-markup t))
    (funcall old-fn last)))

(dolist (fn '(markdown-fontify-blockquotes
              markdown-fontify-hrs
              markdown-fontify-list-items
              markdown-fontify-sub-superscripts))
  (advice-add fn :around 'always-prettify-markdown))

;;; Faces:
(with-eval-after-load 'markdown-mode
  (set-face-attribute 'markdown-header-delimiter-face nil :height 0.7))

;;; Keymaps:
(with-eval-after-load 'markdown-mode
  ;; Make markdown mode always indent / unintent with TAB / S-TAB
  (keymap-set markdown-mode-map "TAB" 'markdown-demote-list-item)
  (keymap-set markdown-mode-map "<backtab>" 'markdown-promote-list-item))
