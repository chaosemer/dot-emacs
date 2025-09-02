;;; init/text.el --- Text mode customizations  -*- lexical-binding: t; -*-

;;; Code:
(add-hook 'text-mode-hook
          (defun my-text-mode-hook ()
            (unless (member major-mode '(mail-mode org-mode))
              visual-line-mode)))

;;; Keymaps:
(with-eval-after-load 'markdown-mode
  ;; Make markdown mode always indent / unintent with TAB / S-TAB
  (keymap-set markdown-mode-map "TAB" 'markdown-demote-list-item)
  (keymap-set markdown-mode-map "<backtab>" 'markdown-promote-list-item))
