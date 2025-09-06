;;; init/text.el --- Text mode customizations  -*- lexical-binding: t; -*-

;;; Code:
(add-hook 'text-mode-hook
          (defun my-text-mode-hook ()
            (unless (member major-mode '(mail-mode org-mode))
              visual-line-mode)))
