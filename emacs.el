;; Global customizations -------------------------------------------------------
(require-noerror 'cua)

(cond ((fboundp 'CUA-mode-on)
       (warn "Using old compatibility mode for CUA")
       (CUA-mode-on))
	  ((fboundp 'cua-mode) (cua-mode 1)))
(mouse-wheel-mode 1)
(column-number-mode 1)
(iswitchb-mode 1)
(tool-bar-mode 0)
(menu-bar-mode (if window-system 1 -1))

(setq-default indent-tabs-mode nil)
(setf frame-title-format "%b - Emacs")
(setf icon-title-format "%b - Emacs")
(setf x-stretch-cursor t)
(setf scroll-conservatively 65535)


(setf (face-background 'show-paren-match-face)
      (if window-system "light gray" "yellow"))
(setf (face-background 'show-paren-mismatch-face) "red"
      (face-foreground 'show-paren-mismatch-face) "white")

;; indent on newline
(global-set-key (kbd "C-j") 'newline)
(global-set-key (kbd "RET") 'newline-and-indent)

(global-set-key (if (featurep 'dos-w32) (kbd "<apps>") (kbd "<menu>"))
                'execute-extended-command)
(when (featurep 'dos-w32)
  (global-set-key (kbd "M-<f4>") 'save-buffers-kill-emacs))
(global-set-key (kbd "C-g") 'goto-line)

(global-set-key (kbd "M-<right>") 'forward-sexp)
(global-set-key (kbd "M-<left>") 'backward-sexp)
(global-set-key (kbd "M-<up>") 'backward-up-list)
(global-set-key (kbd "M-<down>") 'down-list)
(global-set-key (kbd "M-<delete>") 'kill-sexp)

(global-set-key (kbd "<down-mouse-3>") (lambda (event prefix)
                                         (interactive "@e\nP")
                                         (popup-menu menu-bar-edit-menu event prefix)))
;; I'm always mistakenly hitting these, when I do NOT want to scroll left/right
(global-unset-key (kbd "C-<next>"))
(global-unset-key (kbd "C-<prior>"))
