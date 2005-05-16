;; Global customizations -------------------------------------------------------
(cua-mode 1)
(mouse-wheel-mode 1)
(column-number-mode 1)
(iswitchb-mode 1)
(tool-bar-mode -1)
(show-paren-mode 1)
(menu-bar-mode (if window-system 1 -1))

(setf (default-value 'indent-tabs-mode) nil
      (default-value 'truncate-lines) nil
      truncate-partial-width-windows nil
      frame-title-format "%b - Emacs"
      icon-title-format "%b - Emacs")
(setf x-stretch-cursor t)
(setf scroll-conservatively 65535)


(setf (face-background 'show-paren-match-face) (if window-system "light gray" "blue")
      (face-background 'show-paren-mismatch-face) "red"
      (face-foreground 'show-paren-mismatch-face) "white")

;; indent on newline
(setf (global-key-binding (kbd "C-j")) 'newline
      (global-key-binding (kbd "RET")) 'newline-and-indent)

;; usual editor bindings
(setf (global-key-binding (kbd "C-f")) 'occur
      (global-key-binding (kbd "C-S-f")) 'grep-tree
      (global-key-binding (kbd "C-g")) 'goto-line
      (global-key-binding (kbd "<f7>")) 'compile
      (global-key-binding (kbd "S-<f7>")) 'kill-compilation)

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
      (global-key-binding (kbd "M-SPC")) 'mark-sexp      
      (global-key-binding (kbd "M-<delete>")) 'kill-sexp 
      (global-key-binding (kbd "M-<backspace>")) 'backward-kill-sexp)

;; I'm always mistakenly hitting these, when I do NOT want to scroll left/right
(global-unset-key (kbd "C-<next>"))
(global-unset-key (kbd "C-<prior>"))

;; simple prefix-arg functions
(defun find-file-context (filename &optional other-window? wildcards)
  "Calls FIND-FILE or FIND-FILE-OTHER-WINDOW, depending on the prefix arg."
  (interactive (list (read-file-name "Find file: ")
                     current-prefix-arg
                     t))

  (funcall (if other-window? 'find-file-other-window 'find-file)
           filename
           wildcards))
(setf (global-key-binding (kbd "C-x C-f")) 'find-file-context)
