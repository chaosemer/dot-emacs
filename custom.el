;; -*- lexical-binding: t; -*-
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-revert-interval 0.02)
 '(c-default-style "stroustrup")
 '(calculator-electric-mode t)
 '(calculator-number-digits 10)
 '(compilation-ask-about-save nil)
 '(compilation-scroll-output t)
 '(cscope-option-do-not-update-database t)
 '(cursor-in-non-selected-windows nil)
 '(custom-file "~/.config/emacs/init/custom.el")
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(electric-pair-skip-whitespace 'chomp)
 '(goto-address-mail-face 'link)
 '(goto-address-mail-mouse-face 'highlight)
 '(hscroll-step 1)
 '(ido-enable-regexp t)
 '(indicate-empty-lines t)
 '(initial-major-mode 'fundamental-mode)
 '(jit-lock-stealth-time 0.2)
 '(mouse-wheel-progressive-speed nil)
 '(nxml-slash-auto-complete-flag t)
 '(package-selected-packages '(markdown-mode bar-cursor slime))
 '(server-window 'switch-to-buffer-other-frame)
 '(show-paren-style 'expression)
 '(vc-handled-backends '(SVN Git Hg))
 '(visible-cursor nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
