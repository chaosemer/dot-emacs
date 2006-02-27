(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(CUA-mode-overwrite-cursor-color "black")
 '(CUA-mode-read-only-cursor-color "red")
 '(bar-cursor-mode t nil (bar-cursor))
 '(c-basic-offset 4)
 '(c-default-style "stroustrup")
 '(calculator-electric-mode t)
 '(calculator-number-digits 10)
 '(case-fold-search t)
 '(column-number-mode t)
 '(compilation-ask-about-save nil)
 '(compilation-scroll-output t)
 '(current-language-environment "English")
 '(cursor-in-non-selected-windows nil)
 '(custom-file "~/.emacs.d/custom.el")
 '(default-frame-alist (quote ((vertical-scroll-bars . right) (tool-bar-lines . 0) (menu-bar-lines . 1) (width . 131) (height . 58))))
 '(double-click-time 200)
 '(ediff-keep-variants nil)
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(emacs-lisp-mode-hook (quote (turn-on-eldoc-mode semantic-default-elisp-setup)))
 '(face-font-selection-order (quote (:height :width :weight :slant)))
 '(fill-column 100)
 '(flyspell-sort-corrections t)
 '(focus-follows-mouse t)
 '(gdb-many-windows t)
 '(global-font-lock-mode t nil (font-lock))
 '(global-reveal-mode t)
 '(hi-lock-mode t t (hi-lock))
 '(hide-ifdef-initially t)
 '(ielm-mode-hook (quote ((lambda nil (eldoc-mode 1)))))
 '(ilisp-*use-fsf-compliant-keybindings* t)
 '(ilisp-bindings-*bind-right-bracket-p* t)
 '(indicate-empty-lines t)
 '(inhibit-splash-screen t)
 '(jit-lock-stealth-time 0.2)
 '(load-home-init-file t t)
 '(midnight-mode t nil (midnight))
 '(mouse-autoselect-window t)
 '(mouse-wheel-scroll-amount (quote (1 ((control)))))
 '(nxml-sexp-element-flag t)
 '(nxml-slash-auto-complete-flag t)
 '(scroll-bar-mode (quote right))
 '(semantic-idle-scheduler-idle-time 0.5)
 '(show-paren-mode t)
 '(show-paren-style (quote expression))
 '(speedbar-supported-extension-expressions (quote (".[ch]\\(\\+\\+\\|pp\\|c\\|h\\|xx\\)?" ".tex\\(i\\(nfo\\)?\\)?" ".el" ".emacs" ".l" ".lsp" ".lisp" ".lml" ".p" ".java" ".f\\(90\\|77\\|or\\)?" ".ada" ".p[lm]" ".tcl" ".m" ".scm" ".pm" ".py" ".g" ".s?html" "[Mm]akefile\\(\\.in\\)?" ".xml")))
 '(tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100)))
 '(tmm-completion-prompt nil)
 '(transient-mark-mode t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(url-privacy-level (quote paranoid))
 '(vc-cvs-stay-local nil)
 '(w3m-favicon-use-cache-file t)
 '(w3m-use-tab nil)
 '(w3m-use-tab-menubar nil)
 '(x-select-enable-clipboard t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(mode-line-highlight ((((class color) (min-colors 88)) (:background "royal blue" :foreground "white" :box (:line-width 2 :color "steel blue" :style released-button)))))
 '(nxml-comment-content-face ((t (:inherit font-lock-comment-face))))
 '(nxml-comment-delimiter-face ((t (:inherit font-lock-comment-delimiter-face)))))
