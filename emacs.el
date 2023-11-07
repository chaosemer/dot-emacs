;;;; Global Emacs customizations.  -*- lexical-binding: t; -*-
;;;;
;;;; Put stuff here if you have nowhere else to put them
(unless (require 'bar-cursor nil t)
  ;; If this triggers, make sure to install the `bar-cursor' package
  ;; from melpa unstable.
  (display-warning 'emacs "SETUP ISSUE: bar-cursor package is not installed.")
  (defun bar-cursor-mode (&optional arg)
    ;; Do nothing -- stub
    ))

;; Access to the melpa.org packages.
(add-to-list 'package-archives
             '("melpa" . "http://stable.melpa.org/packages/"))

;; Global customizations -----------------------------------------------
(progn (bar-cursor-mode 1)
       (setf minor-mode-alist (assoc-delete-all 'bar-cursor-mode minor-mode-alist)))
(column-number-mode 1)
(context-menu-mode 1)
(cua-mode 1)
(electric-pair-mode 1)
(fido-mode 1)
(global-font-lock-mode 1)
(global-hi-lock-mode 1)
(progn (global-subword-mode 1)
       (setf minor-mode-alist (assoc-delete-all 'subword-mode minor-mode-alist)))
(menu-bar-mode 1)
(progn (pixel-scroll-precision-mode 1)
       (setf pixel-scroll-precision-interpolate-page t))
(recentf-mode 1)
(tooltip-mode -1)
(url-handler-mode 1)

(if window-system
    (mouse-wheel-mode 1)
  (xterm-mouse-mode 1)
  (when (string-match "microsoft" (shell-command-to-string "uname -r"))
    ;; Windows Console does not properly report that it supports
    ;; setSelection. It does not support other functionality.
    (setq xterm-extra-capabilities '(setSelection))

    ;; Use the default Windows browser.
    (setq browse-url-browser-function (lambda (url &rest args)
                                        (call-process "explorer.exe" nil nil nil url)))))
(when (require 'hfyview nil t)
  (hfyview-add-to-files-menu))

(setf (default-value 'indent-tabs-mode) nil
      truncate-partial-width-windows nil
      frame-title-format "%b - Emacs"
      icon-title-format "%b - Emacs"
      x-stretch-cursor t
      scroll-conservatively most-positive-fixnum
      parse-sexp-lookup-properties t
      narrow-to-defun-include-comments t
      frame-resize-pixelwise t
      window-resize-pixelwise t
      use-short-answers t
      use-dialog-box nil
      outline-minor-mode-use-buttons 'in-margins
      delete-by-moving-to-trash t)


;; Customize the *scratch* buffer
(hook-mode emacs-startup-hook
  (with-current-buffer (get-buffer "*scratch*")
    (setf buffer-offer-save t)))
(setf initial-major-mode 'gfm-mode
      initial-scratch-message (concat "Scratch buffer for notes\n"
                                      "========================\n"
                                      "Put notes here..."))

;; When deugging xterm-mouse issues, having a large buffer is quite
;; helpful.
(lossage-size 10000)

(setf (face-background 'show-paren-match)
      (if (> (display-color-cells) 256) "light gray" "blue")
      (face-background 'show-paren-mismatch) "red"
      (face-foreground 'show-paren-mismatch) "white")

;; indent on newline
(keymap-global-set "C-j" 'newline)
(keymap-global-set "RET" 'newline-and-indent)

;; usual editor bindings
(keymap-global-set "C-f" 'occur)
(keymap-global-set "C-S-f" 'rgrep)
(keymap-global-set "C-g" 'goto-line)
(keymap-global-set "<f7>" 'recompile)
(keymap-global-set "C-<f7>" 'compile)
(keymap-global-set "S-<f7>" 'kill-compilation)
(keymap-global-set "C-a" 'mark-whole-buffer)
(keymap-global-set "M-<home>" 'beginning-of-defun)
(keymap-global-set "M-<end>" 'end-of-defun)

;; Window system integration
(when window-system
  (keymap-global-set "<menu>" 'execute-extended-command)
  (keymap-global-set "S-<menu>" 'eval-expression)
  (when (featurep 'dos-w32)
    (keymap-global-set "M-<f4>"
                       (lambda ()
                         (interactive)
                         (if (> (length (frame-list)) 1)
                             (delete-frame)
                           (when (y-or-n-p "Last frame, kill emacs? ")
                             (call-interactively #'save-buffers-kill-emacs)))))))

;; Account for differences in Win32 keycodes
(keymap-set key-translation-map "C-<tab>" (key-parse "M-TAB"))

;; Handle different platforms diving differnt names to the same key
(when (featurep 'dos-w32)
  (keymap-set function-key-map "<apps>" (key-parse "<menu>"))
  (keymap-set function-key-map "S-<apps>" (key-parse "S-<menu>")))
(when (and (null window-system) (string= (getenv "TERM") "xterm"))
  (keymap-set function-key-map "<print>" (key-parse "<menu>"))
  (keymap-set function-key-map "S-<print>" (key-parse "S-<menu>")))

;; simpler sexp bindings
(keymap-global-set "M-<right>" 'forward-sexp)
(keymap-global-set "M-<left>" 'backward-sexp)
(keymap-global-set "M-<up>" 'backward-up-list)
(keymap-global-set "M-<down>" 'down-list)
(keymap-global-set "M-SPC" 'mark-sexp)
(keymap-global-set "M-<delete>" 'kill-sexp)
(keymap-global-set "M-<backspace>" 'backward-kill-sexp)

;; I'm always mistakenly hitting these
(dolist (key '("C-<next>" "C-<prior>" "C-x m" "M-<home>" "M-<end>" "M-<begin>" "C-x <left>"
               "C-x <right>" "M-<begin>" "M-<next>" "M-<prior>" "C-M-v" "C-M-S-v" "ESC <begin>"
               "ESC <end>" "ESC <home>" "ESC <next>" "ESC <prior>"))
  (keymap-global-unset key))

;;; DWIM <home> and <end> TODO(package)
(defun beginning-of-line-dwim (&optional n)
  "Move point to the first non-whitespace character or the beginning of line."
  (interactive "^p")

  (let ((point (point)))
    (back-to-indentation)
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

(keymap-global-set "<home>" 'beginning-of-line-dwim)
(keymap-global-set "<end>" 'end-of-line-dwim)

;;; Recursive edits TODO(package)
(defun push-or-pop-excursion (pop?)
  "Pushes or pops an excursion, depending on the prefix arg."
  (interactive (list current-prefix-arg))

  (if (not pop?)
      (save-excursion (save-restriction (save-window-excursion (recursive-edit))))
    (when (> (recursion-depth) 0)
      (throw 'exit 'nil))))
(keymap-global-set "C-x C-p" 'push-or-pop-excursion)

;;; Section and File level comment functionality.
;;;
;;; This allows for section and file level comments to be rendered bigger than normal text, so they
;;; stand out a bit more.
(defface section-comment-face
  '((t (:height 1.3 :inherit (font-lock-comment-face variable-pitch))))
  "Face for section level comments"
  :group 'local)
(defface file-comment-face
  '((t (:height 1.5 :weight bold :inherit (font-lock-comment-face variable-pitch))))
  "Face for file level comments"
  :group 'local)

;; Also make same adjustments for markdown.
(setf markdown-header-scaling t)

;;; Sibling file navigation TODO(upstream)
(defun my-find-sibling-file-other-window (file)
  (interactive (progn
                 (unless buffer-file-name
                   (user-error "Not visiting a file"))
                 (list buffer-file-name)))
  (other-window-prefix)
  (unwind-protect
      (find-sibling-file file)
    (let ((this-command t))
      (run-hooks 'post-command-hook))))
(defun my-find-sibling-file-other-frame (file)
  (interactive (progn
                 (unless buffer-file-name
                   (user-error "Not visiting a file"))
                 (list buffer-file-name)))
  (other-frame-prefix)
  (unwind-protect
      (find-sibling-file file)
    (let ((this-command t))
      (run-hooks 'post-command-hook))))
(setf find-sibling-rules
      '(("\\([^/]+\\)\\.c$" "\\1.h")
        ("\\([^/]+\\)\\.cc$" "\\1.h" "\\1.hh")
        ("\\([^/]+\\)\\.cpp$" "\\1.h" "\\1.hpp")
        ("\\([^/]+\\)\\.h$" "\\1.c" "\\1.cpp" "\\1.cc")
        ("\\([^/]+\\)\\.hh$" "\\1.cc")
        ("\\([^/]+\\)\\.hpp$" "\\1.cpp")))

(keymap-global-set "C-x C-h" 'find-sibling-file)
(keymap-global-set "C-x 4 C-h" 'my-find-sibling-file-other-window)
(keymap-global-set "C-x 4 h" 'my-find-sibling-file-other-window)
(keymap-global-set "C-x 5 C-h" 'my-find-sibling-file-other-frame)
(keymap-global-set "C-x 5 h" 'my-find-sibling-file-other-frame)

;;; Other misc stuff TODO(package)
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
(keymap-global-set "TAB" 'indent-dwim)
