(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-default-style "stroustrup")
 '(calculator-electric-mode t)
 '(calculator-number-digits 10)
 '(compilation-ask-about-save nil)
 '(compilation-scroll-output t)
 '(cscope-option-do-not-update-database t)
 '(cursor-in-non-selected-windows nil)
 '(custom-file "~/.emacs.d/init/custom.el")
 '(default-frame-alist
    (quote
     ((tool-bar-lines . 0)
      (menu-bar-lines . 1))))
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(electric-pair-skip-whitespace (quote chomp))
 '(gdb-many-windows t)
 '(goto-address-mail-face (quote link))
 '(goto-address-mail-mouse-face (quote highlight))
 '(gud-tooltip-mode t)
 '(hide-comments-faces
   (quote
    (font-lock-comment-face font-lock-comment-delimiter-face section-comment-face file-comment-face)) t)
 '(hscroll-step 1)
 '(ido-enable-regexp t)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(initial-major-mode (quote fundamental-mode))
 '(jit-lock-stealth-time 0.2)
 '(load-home-init-file t t)
 '(mouse-autoselect-window t)
 '(mouse-wheel-progressive-speed nil)
 '(nxml-sexp-element-flag t)
 '(nxml-slash-auto-complete-flag t)
 '(read-quoted-char-radix 16)
 '(recentf-mode t)
 '(server-window (quote switch-to-buffer-other-frame))
 '(show-paren-mode t)
 '(show-paren-style (quote expression))
 '(tab-stop-list
   (quote
    (4 8 12)))
 '(tmm-completion-prompt nil)
 '(tooltip-mode nil)
 '(url-handler-mode t)
 '(vc-handled-backends (quote (CVS SVN Git))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width normal :foundry "unknown" :family "Source Code Pro"))))
 '(mode-line-highlight ((((class color) (min-colors 88)) (:background "royal blue" :foreground "white" :box (:line-width 2 :color "steel blue" :style released-button))))))
