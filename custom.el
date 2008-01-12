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
 '(Man-section-translations-alist (quote (("3C++" . "3") ("3X11" . "3") ("1-UCB" . "") ("3G" . "3"))))
 '(c-basic-offset 4)
 '(c-comment-prefix-regexp (quote ((pike-mode . "//+!?\\|\\**") (awk-mode . "#+") (other . "//+<?\\|\\**"))))
 '(c-default-style "stroustrup")
 '(calculator-electric-mode t)
 '(calculator-number-digits 10)
 '(case-fold-search t)
 '(compilation-ask-about-save nil)
 '(compilation-scroll-output t)
 '(current-language-environment "English")
 '(cursor-in-non-selected-windows nil)
 '(custom-file "~/.emacs.d/custom.el")
 '(default-frame-alist (quote ((vertical-scroll-bars . right) (tool-bar-lines . 0) (menu-bar-lines . 1))))
 '(double-click-time 200)
 '(ediff-keep-variants nil)
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(emacs-lisp-mode-hook (quote (turn-on-eldoc-mode semantic-default-elisp-setup)))
 '(enable-recursive-minibuffers t)
 '(face-font-selection-order (quote (:height :width :weight :slant)))
 '(flyspell-sort-corrections t)
 '(focus-follows-mouse t)
 '(gdb-many-windows t)
 '(gdb-use-separate-io-buffer t)
 '(global-reveal-mode t)
 '(gnuserv-mode-line-string nil)
 '(goto-address-mail-face (quote link))
 '(goto-address-mail-mouse-face (quote highlight))
 '(goto-address-url-face (quote link))
 '(grep-files-aliases (quote (("el" . "*.el") ("ch" . "*.[ch]") ("c" . "*.c") ("h" . "*.h") ("asm" . "*.[sS]") ("m" . "[Mm]akefile*") ("l" . "[Cc]hange[Ll]og*") ("c++" . "*.[ch] *.[ch]pp"))))
 '(gud-tooltip-echo-area t)
 '(gud-tooltip-mode t)
 '(haskell-program-name "ghci")
 '(hide-comments-faces (quote (font-lock-comment-face font-lock-comment-delimiter-face section-comment-face file-comment-face)))
 '(hide-ifdef-initially t)
 '(ido-save-directory-list-file "~/.emacs.d/ido.last")
 '(ielm-mode-hook (quote ((lambda nil (eldoc-mode 1)))))
 '(ilisp-*use-fsf-compliant-keybindings* t)
 '(ilisp-bindings-*bind-right-bracket-p* t)
 '(indicate-empty-lines t)
 '(inhibit-splash-screen t)
 '(initial-major-mode (quote fundamental-mode))
 '(jit-lock-stealth-time 0.2)
 '(load-home-init-file t t)
 '(minibuffer-prompt-properties (quote (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))
 '(mouse-autoselect-window t)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (3 ((control)))))
 '(nxml-sexp-element-flag t)
 '(nxml-slash-auto-complete-flag t)
 '(read-quoted-char-radix 16)
 '(recentf-keep (quote (file-remote-p file-readable-p)))
 '(recentf-menu-before "Insert File...")
 '(recentf-mode t)
 '(recentf-save-file "~/.emacs.d/recentf")
 '(safe-local-eval-forms (quote ((hide-body))))
 '(safe-local-variable-values (quote ((hide-ifdef-initially))))
 '(scroll-bar-mode (quote right))
 '(semantic-idle-scheduler-idle-time 0.5)
 '(server-window (quote switch-to-buffer-other-frame))
 '(show-paren-mode t)
 '(show-paren-style (quote expression))
 '(speedbar-supported-extension-expressions (quote (".[ch]\\(\\+\\+\\|pp\\|c\\|h\\|xx\\)?" ".tex\\(i\\(nfo\\)?\\)?" ".el" ".emacs" ".l" ".lsp" ".lisp" ".lml" ".p" ".java" ".f\\(90\\|77\\|or\\)?" ".ada" ".p[lm]" ".tcl" ".m" ".scm" ".pm" ".py" ".g" ".s?html" "[Mm]akefile\\(\\.in\\)?" ".xml")))
 '(tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100)))
 '(tmm-completion-prompt nil)
 '(tooltip-delay 0)
 '(tooltip-mode t)
 '(tooltip-use-echo-area t)
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
