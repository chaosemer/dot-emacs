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
(setf (global-key-binding (kbd "C-j")) 'newline
      (global-key-binding (kbd "RET")) 'newline-and-indent)

;; usual editor bindings
(setf (global-key-binding (kbd "C-f")) 'occur
      (global-key-binding (kbd "C-S-f")) 'grep-tree
      (global-key-binding (kbd "C-g")) 'goto-line)

;; Window system integration
(when window-system
  (setf (global-key-binding (kbd "<menu>")) 'execute-extended-command
        (global-key-binding (kbd "<apps>")) 'execute-extended-command
        (global-key-binding (kbd "<down-mouse-3>")) (lambda (event prefix)
                                                      (interactive "@e\np")
                                                      (popup-menu menu-bar-edit-menu event prefix)))
  (when (featurep 'dos-w32)
    (setf (global-key-binding (kbd "M-<f4>")) 'save-buffers-kill-emacs)))

;; simpler sexp bindings
(setf (global-key-binding (kbd "M-<right>")) 'forward-sexp
      (global-key-binding (kbd "M-<left>")) 'backward-sexp
      (global-key-binding (kbd "M-<up>")) 'backward-up-list
      (global-key-binding (kbd "M-<down>")) 'down-list
      (global-key-binding (kbd "M-<delete>")) 'kill-sexp)

;; I'm always mistakenly hitting these, when I do NOT want to scroll left/right
(global-unset-key (kbd "C-<next>"))
(global-unset-key (kbd "C-<prior>"))
