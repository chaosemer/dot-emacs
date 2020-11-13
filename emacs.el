;;;; Global Emacs customizations.  -*- lexical-binding: t; -*-
;;;;
;;;; Put stuff here if you have nowhere else to put them
(require 'bar-cursor)

;; Access to the melpa.org packages.
(add-to-list 'package-archives
             '("melpa" . "http://stable.melpa.org/packages/"))

;; Global customizations -----------------------------------------------
(global-subword-mode 1)
(assoc-delete-all 'subword-mode minor-mode-alist)
(bar-cursor-mode 1)
(assoc-delete-all 'bar-cursor-mode minor-mode-alist)

(column-number-mode 1)
(cua-mode 1)
(global-hi-lock-mode 1)
(menu-bar-mode 1)
(if window-system
    (mouse-wheel-mode 1)
  (xterm-mouse-mode 1))
(show-paren-mode 1)
(tool-bar-mode -1)
(global-font-lock-mode 1)
(electric-pair-mode 1)
(url-handler-mode 1)
(recentf-mode 1)
(when (require-noerror 'hfyview)
  (hfyview-add-to-files-menu))
(hook-mode emacs-startup-hook
  (with-current-buffer (get-buffer "*scratch*")
    (setf buffer-offer-save t)))

(setf (default-value 'indent-tabs-mode) nil
      truncate-partial-width-windows nil
      frame-title-format "%b - Emacs"
      icon-title-format "%b - Emacs"
      x-stretch-cursor t
      scroll-conservatively most-positive-fixnum
      parse-sexp-lookup-properties t
      narrow-to-defun-include-comments t)

(setf (face-background 'show-paren-match)
      (if (> (display-color-cells) 256) "light gray" "blue")
      (face-background 'show-paren-mismatch) "red"
      (face-foreground 'show-paren-mismatch) "white")

;; indent on newline
(setf (global-key-binding (kbd "C-j")) 'newline
      (global-key-binding (kbd "RET")) 'newline-and-indent)

;; usual editor bindings
(setf (global-key-binding (kbd "C-f")) 'occur
      (global-key-binding (kbd "C-S-f")) 'rgrep
      (global-key-binding (kbd "C-g")) 'goto-line
      (global-key-binding (kbd "<f7>")) 'recompile
      (global-key-binding (kbd "C-<f7>")) 'compile
      (global-key-binding (kbd "S-<f7>")) 'kill-compilation
      (global-key-binding (kbd "C-a")) 'mark-whole-buffer
      (global-key-binding (kbd "M-<home>")) 'beginning-of-defun
      (global-key-binding (kbd "M-<end>")) 'end-of-defun)

;; some more consistant key bindings
(setf (global-key-binding (kbd "C-x 4 <next>")) 'scroll-other-window
      (global-key-binding (kbd "C-x 4 <prior>")) 'scroll-other-window-down)

;; Window system integration
(when window-system
  (setf (global-key-binding (kbd "<menu>")) 'execute-extended-command
        (global-key-binding (kbd "S-<menu>")) 'eval-expression)
  (when (featurep 'dos-w32)
    (setf (global-key-binding (kbd "M-<f4>"))
          (lambda ()
            (interactive)
            (if (> (length (frame-list)) 1)
                (delete-frame)
              (when (y-or-n-p "Last frame, kill emacs? ")
                (call-interactively #'save-buffers-kill-emacs )))))))

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

;; Change display for characters not displayable on Windows console.
(when (string-match "microsoft" (shell-command-to-string "uname -r"))
  (aset (or standard-display-table
            (setq standard-display-table (make-display-table)))
        ?⇒                             ; RIGHTWARDS DOUBLE ARROW
        (vector (make-glyph-code ?= 'homoglyph)
                (make-glyph-code ?> 'homoglyph))))

;; simpler sexp bindings
(setf (global-key-binding (kbd "M-<right>")) 'forward-sexp
      (global-key-binding (kbd "M-<left>")) 'backward-sexp
      (global-key-binding (kbd "M-<up>")) 'backward-up-list
      (global-key-binding (kbd "M-<down>")) 'down-list
      (global-key-binding (kbd "M-SPC")) 'mark-sexp
      (global-key-binding (kbd "M-<delete>")) 'kill-sexp
      (global-key-binding (kbd "M-<backspace>")) 'backward-kill-sexp)

;; I'm always mistakenly hitting these
(dolist (key '("C-<next>" "C-<prior>" "C-x m" "M-<home>" "M-<end>" "M-<begin>" "C-x <left>"
               "C-x <right>" "M-<begin>" "M-<next>" "M-<prior>" "C-M-v" "C-M-S-v" "ESC <begin>"
               "ESC <end>" "ESC <home>" "ESC <next>" "ESC <prior>"))
  (setf (global-key-binding (read-kbd-macro key)) nil))

;;; DWIM <home> and <end> TODO(package)
(defun beginning-of-line-dwim (&optional n)
  "Move point to the first non-whitespace character or the beginning of line."
  (interactive "^p")

  (let ((point (point)))
    (beginning-of-line n)
    (skip-chars-forward " \t")
    (when (= point (point))
      (beginning-of-line))))

(defun end-of-line-dwim (&optional n)
  "Movie point to the last non-whitespace character or the end of line."
  (interactive "^p")

  (let ((point (point)))
    (end-of-line n)
    (skip-chars-backward " \t")
    (when (= point (point))
      (end-of-line))))

(setf (global-key-binding (kbd "<home>")) 'beginning-of-line-dwim
      (global-key-binding (kbd "<end>")) 'end-of-line-dwim)

;;; Recursive edits TODO(package)
(defun push-or-pop-excursion (pop?)
  "Pushes or pops an excursion, depending on the prefix arg."
  (interactive (list current-prefix-arg))

  (if (not pop?)
      (save-excursion (save-restriction (save-window-excursion (recursive-edit))))
    (when (> (recursion-depth) 0)
      (throw 'exit 'nil))))
(setf (global-key-binding (kbd "C-x C-p")) 'push-or-pop-excursion)

;;; Section and File level comment functionality.
;;;
;;; This allows for section and file level comments to be rendered bigger than normal text, so they
;;; stand out a bit more.
(defface section-comment-face
  '((t (:height 1.2 :inherit font-lock-comment-face)))
  "Face for section level comments"
  :group 'local)
(defface file-comment-face
  '((t (:height 1.5 :weight bold :inherit font-lock-comment-face)))
  "Face for file level comments"
  :group 'local)

;;; Pair file navigation TODO(package)
;;;
;;; Many programming languages have the concept of two related files, like C's source and header
;;; files.  This allows you to navigate between two pair files with the press of a key.
;;;
;;; NOTE: This is somewhat duplicate functionality to ff-find-other-file.  Mine supports finding in
;;; other frames and other windows, for convience, while ff-find-other-file does not.
(defvar pair-file-extension-alist
  ;; Can't allow literal data to be modified...
  (copy-tree '(("c" "h")
               ("h" "c" "cpp")
               ("cpp" "hpp" "h")
               ("hpp" "cpp")))
  "*Alist of extensions mapped to potential pair extensions.")

(defun pair-file-list (filename)
  "Return a list of all potential pair files for filename,
ordered by preference

Pair files are determined by replacing the extension of FILENAME
with each extension listed in `pair-file-extension-alist' for
that extension."
  (let ((extensions (cdr (assoc (file-name-extension filename)
                                pair-file-extension-alist
                                #'string=))))
    (cl-loop for extension in extensions
             collect (format "%s.%s"
                             (file-name-sans-extension filename)
                             extension))))

(cl-defun find-pair-file-noselect (filename)
  "Read the pair file of FILENAME into a buffer and return that
buffer.  See also `find-file-noselect'."
  (unless filename
    (error "Buffer is not visiting a file"))
  (let ((files (pair-file-list filename)))
    (when files
      (dolist (file files)
        (when (or (file-exists-p file)
                  (find-buffer-visiting file))
          (cl-return-from find-pair-file-noselect
            (find-file-noselect file)))))
    (error "No known pair for file %s" filename)))

(defun find-pair-file-read-args (prompt)
  (list (read-file-name prompt nil buffer-file-name)))

(defun find-pair-file (filename)
  "Edit the pair file of FILENAME.

Pair files are determined by `pair-file-list'."
  (interactive (find-pair-file-read-args "Find pair file of: "))
  (switch-to-buffer (find-pair-file-noselect filename)))
(defun find-pair-file-other-window (filename)
  "Edit the pair file of FILENAME in another window.

Pair files are determined by `pair-file-list'."
  (interactive (find-pair-file-read-args "Find pair file in other window of: "))
  (switch-to-buffer-other-window (find-pair-file-noselect filename)))
(defun find-pair-file-other-frame (filename)
  "Edit the pair file of FILENAME in another frame.

Pair files are determined by `pair-file-list'."
  (interactive (find-pair-file-read-args "Find pair file in other frame of: "))
  (switch-to-buffer-other-frame (find-pair-file-noselect filename)))

(defun switch-to-pair-file (&optional createp)
  "Display the pair file of the current file in the same window.  If CREATEP is

Pair files are determined by `pair-file-list'."
  (interactive)
  (find-pair-file buffer-file-name))
(defun switch-to-pair-file-other-window ()
  "Display the pair file of the current file in another window.

Pair files are determined by `pair-file-list'."
  (interactive)
  (find-pair-file-other-window buffer-file-name))
(defun switch-to-pair-file-other-frame ()
  "Display the pair file of the current file in another frame.

Pair files are determined by `pair-file-list'."
  (interactive)
  (find-pair-file-other-frame buffer-file-name))

(setf (global-key-binding (kbd "C-x C-h")) #'switch-to-pair-file
      (global-key-binding (kbd "C-x 4 C-h")) #'switch-to-pair-file-other-window
      (global-key-binding (kbd "C-x 4 h")) #'switch-to-pair-file-other-window
      (global-key-binding (kbd "C-x 5 C-h")) #'switch-to-pair-file-other-frame
      (global-key-binding (kbd "C-x 5 h")) #'switch-to-pair-file-other-frame)

;;; Other misc stuff TODO(package)
(defun scratch ()
  "Switch to the scratch buffer."
  (interactive)
  (display-buffer (get-buffer-create "*scratch*") nil t))

(defun indent-dwim (arg)
  "Try to do what a human would mean when indenting.

The prefix argument, if given, indents to that column"
  (interactive (list current-prefix-arg))

  (cond (mark-active
         (indent-region (region-beginning) (region-end) arg))
        (arg
         (save-excursion
           (beginning-of-line)
           (delete-horizontal-space)
           (indent-to (prefix-numeric-value arg))))
        (t
         (indent-according-to-mode))))
(setf (global-key-binding (kbd "TAB")) 'indent-dwim)
