(require 'camelCase)
(require 'compile)
(require 'dirvars)
(require 'hrule)
(require 'tramp)

;; Global customizations -------------------------------------------------------
(setf (lookup-key camelCase-mode-map (kbd "M-<left>")) nil
      (lookup-key camelCase-mode-map (kbd "M-<right>")) nil
      (lookup-key camelCase-mode-map (kbd "M-<delete>")) nil
      (lookup-key camelCase-mode-map (kbd "M-<backspace>")) nil)
(camelCase-mode 1)
(column-number-mode 1)
(column-number-mode 1)
(cua-mode 1)
(hrule-mode 1)
(iswitchb-mode 1)
(iswitchb-mode 1)
(menu-bar-mode (if window-system 1 -1))
(mouse-wheel-mode 1)
(show-paren-mode 1)
(tool-bar-mode -1)
(tooltip-mode -1)

(defalias 'read-buffer 'iswitchb-read-buffer)

(setf (default-value 'indent-tabs-mode) nil
      (default-value 'truncate-lines) nil
      truncate-partial-width-windows nil
      frame-title-format "%b - Emacs"
      icon-title-format "%b - Emacs"
      x-stretch-cursor t
      scroll-conservatively most-positive-fixnum)

(when (require-noerror 'fringe)
  (set-fringe-mode nil)
  (when (fboundp 'set-fringe-indicators-1)
    (set-fringe-indicators-1 nil 'empty)))

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
      (global-key-binding (kbd "<f7>")) 'recompile
      (global-key-binding (kbd "C-<f7>")) 'compile
      (global-key-binding (kbd "S-<f7>")) 'kill-compilation)

;; Window system integration
(when window-system
  (setf (global-key-binding (kbd "<menu>")) 'execute-extended-command
        (global-key-binding (kbd "S-<menu>")) 'eval-expression
        (global-key-binding (kbd "<down-mouse-3>")) (lambda (event prefix)
                                                      (interactive "@e\np")
                                                      (popup-menu menu-bar-edit-menu event prefix)))
  (when (featurep 'dos-w32)
    (setf (global-key-binding (kbd "M-<f4>")) (lambda ()
                                                (interactive)
                                                (if (> (length (frame-list)) 1)
                                                    (delete-frame)
                                                  (when (y-or-n-p "Last frame, kill emacs? ")
                                                    (call-interactively #'save-buffers-kill-emacs )))))))

;; Completions in other places
; This doesn't work on every other Emacs other than 21.4, debian.  hmmm...
;(setf (lookup-key minibuffer-local-map (kbd "<tab>")) 'hippie-expand)

;; Account for differences in Win32 keycodes
(setf (lookup-key key-translation-map (kbd "C-<tab>")) (kbd "M-TAB"))

;; Handle different platforms diving differnt names to the same key
(when (featurep 'dos-w32)
  (setf (lookup-key function-key-map (kbd "<apps>")) (kbd "<menu>")
        (lookup-key function-key-map (kbd "S-<apps>")) (kbd "S-<menu>")))
(when (and (null window-system) (string= (getenv "TERM") "xterm"))
  (xterm-mouse-mode 1)
  (setf (lookup-key function-key-map (kbd "<print>")) (kbd "<menu>")
        (lookup-key function-key-map (kbd "S-<print>")) (kbd "S-<menu>")))

;; simpler sexp bindings
(setf (global-key-binding (kbd "M-<right>")) 'forward-sexp
      (global-key-binding (kbd "M-<left>")) 'backward-sexp
      (global-key-binding (kbd "M-<up>")) 'backward-up-list
      (global-key-binding (kbd "M-<down>")) 'down-list
      (global-key-binding (kbd "M-SPC")) 'mark-sexp
      (global-key-binding (kbd "M-<delete>")) 'kill-sexp
      (global-key-binding (kbd "M-<backspace>")) 'backward-kill-sexp)

;; I'm always mistakenly hitting these
(setf (global-key-binding (kbd "C-<next>")) nil
      (global-key-binding (kbd "C-<prior>")) nil
      (global-key-binding (kbd "C-x m")) nil
      (global-key-binding (kbd "M-<home>")) nil
      (global-key-binding (kbd "M-<end>")) nil)

;; Recursive edits
(defun push-or-pop-excursion (pop?)
  "Pushes or pops an excursion, depending on the prefix arg."
  (interactive (list current-prefix-arg))

  (if (not pop?)
      (save-excursion (save-restriction (save-window-excursion (recursive-edit))))
    (when (> (recursion-depth) 0)
      (throw 'exit 'nil))))
(setf (global-key-binding (kbd "C-x C-p")) 'push-or-pop-excursion)

;; simple prefix-arg functions
(defun find-file-context (filename &optional other-window? wildcards)
  "Calls `find-file' or `find-file-other-window', depending on the prefix arg."
  (interactive (list (read-file-name "Find file: ")
                     current-prefix-arg
                     t))

  (funcall (if other-window? 'find-file-other-window 'find-file)
           filename
           wildcards))
(setf (global-key-binding (kbd "C-x C-f")) 'find-file-context)
