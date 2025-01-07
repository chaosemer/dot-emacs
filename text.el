;;; init/text.el --- Text mode customizations  -*- lexical-binding: t; -*-

;;; Code:
(add-hook 'text-mode-hook
          (defun my-text-mode-hook ()
            (unless (member major-mode '(mail-mode org-mode))
              visual-line-mode)))

(add-hook 'markdown-mode-hook
          (defun my-markdown-mode-hook ()
            ;; Normally, electric pair mode is on and helpful, but in
            ;; Markdown mode it makes URLs be automatically hidden
            ;; before they're typed in.
            (electric-pair-local-mode -1)
            (markdown-toggle-markup-hiding 1)))

;; Actually, I prefer GitHub flavored markdown
(add-to-list 'major-mode-remap-alist '(markdown-mode . gfm-mode))

;; Make sure we delete selection on highlight.
(put 'markdown-enter-key 'delete-selection t)

(setf markdown-fontify-code-blocks-natively t)

;;; Keymaps:
(with-eval-after-load 'markdown-mode
  ;; Make markdown mode always indent / unintent with TAB / S-TAB
  (keymap-set markdown-mode-map "TAB" 'markdown-demote-list-item)
  (keymap-set markdown-mode-map "<backtab>" 'markdown-promote-list-item))
