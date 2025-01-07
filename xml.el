;;; init/xml.el --- Configuration for XML files  -*- lexical-binding: t; -*-

;;; Code:
(add-to-list 'major-mode-remap-alist '(xml-mode . nxml-mode))
(add-to-list 'major-mode-remap-alist '(mhtml-mode . nxml-mode))

(add-hook 'nxml-mode-hook
          (defun my-nxml-mode-hook ()
            ;; key bindings
            (keymap-local-set "M-<up>" 'nxml-backward-up-element)
            (keymap-local-set "M-<down>" 'nxml-down-element)))
