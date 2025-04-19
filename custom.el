;;; init/custom.el --- Location for changes from Custom. -*- lexical-binding: t; -*-

;;; Code:
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
 '(electric-pair-skip-whitespace 'chomp)
 '(goto-address-mail-face 'link)
 '(goto-address-mail-mouse-face 'highlight)
 '(hscroll-step 1)
 '(ido-enable-regexp t)
 '(indicate-empty-lines t)
 '(jit-lock-stealth-time 0.2)
 '(minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
 '(mouse-wheel-progressive-speed nil)
 '(nxml-slash-auto-complete-flag t)
 '(package-selected-packages
   '(bar-cursor diff-hl dired-icon form-feed-st init-dir markdown-mode slime
                window-tool-bar))
 '(package-vc-selected-packages
   '((window-tool-bar :vc-backend Git :url
                      "http://github.com/chaosemer/window-tool-bar")
     (init-dir :vc-backend Git :url
               "http://github.com/chaosemer/init-dir")))
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
