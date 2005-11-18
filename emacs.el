(require 'dirvars)
(require-noerror 'gnuserv-compat)

;; Global customizations -------------------------------------------------------
(setf (lookup-key camelCase-mode-map (kbd "M-<left>")) nil
      (lookup-key camelCase-mode-map (kbd "M-<right>")) nil
      (lookup-key camelCase-mode-map (kbd "M-<delete>")) nil
      (lookup-key camelCase-mode-map (kbd "M-<backspace>")) nil)
(camelCase-mode 1)
(column-number-mode 1)
(column-number-mode 1)
(cua-mode 1)
(hi-lock-mode 1)
(hrule-mode 1)
(iswitchb-mode 1)
(iswitchb-mode 1)
(menu-bar-mode (if window-system 1 -1))
(mouse-wheel-mode 1)
(show-paren-mode 1)
(tool-bar-mode -1)
(tooltip-mode -1)
(global-font-lock-mode 1)
(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(global-semantic-idle-summary-mode 1)
(global-semantic-show-parser-state-mode 1)
(global-semantic-show-unmatched-syntax-mode 1)

(hook-minor-mode semantic-init-hooks
  (setf (local-key-binding (kbd "M-TAB")) 'semantic-ia-complete-symbol
        (local-key-binding (kbd "M-.")) 'semantic-complete-jump))
(defalias 'read-buffer 'iswitchb-read-buffer)

(setf (default-value 'indent-tabs-mode) nil
      (default-value 'truncate-lines) nil
      truncate-partial-width-windows nil
      frame-title-format "%b - Emacs"
      icon-title-format "%b - Emacs"
      x-stretch-cursor t
      scroll-conservatively most-positive-fixnum
      (second (assoc 'hi-lock-mode minor-mode-alist)) nil   ; do not display hi-lock in modeline
      )

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

;; Spellcheck on save
;(add-hook 'before-save-hook #'ispell-buffer)
;(setf kill-emacs-hook (remove 'ede-save-cache kill-emacs-hook)) ; There seems to be a problem with
;                                                                ; and spellchecking on save.  So I'm
;                                                                ; going to remove EDE for now


;; DWIM <home> and <end>
(defun beginning-of-line-dwim (&optional n)
  "Move point to the first non-whitespace character or the beginning of line."
  (interactive "p")

  (let ((point (point)))
    (beginning-of-line n)
    (skip-chars-forward " \t")
    (when (= point (point))
      (beginning-of-line))))
(setf (get 'beginning-of-line-dwim 'CUA) 'move)

(defun end-of-line-dwim (&optional n)
  "Movie point to the last non-whitespace character or the end of line."
  (interactive "p")

  (let ((point (point)))
    (end-of-line n)
    (skip-chars-backward " \t")             
    (when (= point (point))
      (end-of-line))))
(setf (get 'end-of-line-dwim 'CUA) 'move)

(setf (global-key-binding (kbd "<home>")) 'beginning-of-line-dwim        
      (global-key-binding (kbd "<end>")) 'end-of-line-dwim)

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

;; Update semantic-show-parser-state-marker
(require 'semantic-util-modes)
(setf (get 'semantic-show-parser-state-string 'risky-local-variable) t)
(defun semantic-show-parser-state-marker (&rest ignore)
  "Set `semantic-show-parser-state-string' to indicate parser state.
This marker is one of the following:
 `-'  ->  The cache is up to date.
 `!'  ->  The cache requires a full update.
 `~'  ->  The cache needs to be incrementally parsed.
 `%'  ->  The cache is not currently parseable.
 `@'  ->  Auto-parse in progress (not set here.)
Arguments IGNORE are ignored, and accepted so this can be used as a hook
in many situations."
  (labels ((make-state-string (string &optional (help-echo "") func)
             (if func
                 (let ((map (make-sparse-keymap)))
                   (setf (lookup-key map (kbd "<mode-line> <mouse-1>")) func)
                   (propertize string
                               'help-echo help-echo
                               'mouse-face 'mode-line-highlight
                               'local-map map))
               (propertize string 'help-echo help-echo))))
    (setf semantic-show-parser-state-string
          (cond ((semantic-parse-tree-needs-rebuild-p) 
                 (make-state-string "!" "Needs a full parse: mouse-1 reparses"
                                    (lambda () (interactive) (semantic-refresh-tags-safe) nil)))
                 ((semantic-parse-tree-needs-update-p)
                  (make-state-string "^" "Needs an incremental parse: mouse-1 reparses"
                                     (lambda () (interactive) (semantic-refresh-tags-safe) nil)))
                 ((semantic-parse-tree-unparseable-p)
                  (make-state-string "%" "Buffer Unparsable: mouse-1 reparses"
                                     (lambda () (interactive) (semantic-refresh-tags-safe) nil)))
                 (t
                  (make-state-string "-" "Semantic is up to date")))
              ))
  ;;(message "Setup mode line indicator to [%s]" semantic-show-parser-state-string)
  (semantic-mode-line-update))
