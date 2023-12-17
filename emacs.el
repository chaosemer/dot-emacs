;;; init/emacs.el --- Global Emacs customizations  -*- lexical-binding: t; -*-

;;; Code:
(unless (fboundp 'bar-cursor-mode)
  ;; If this triggers, make sure to install the `bar-cursor' package
  ;; from melpa unstable.
  (display-warning 'emacs "SETUP ISSUE: bar-cursor package is not installed.")
  (defun bar-cursor-mode (&optional _)
    ;; Do nothing -- stub
    ))
(unless (fboundp 'global-form-feed-st-mode)
  ;; If this triggers, make sure to install the `form-feed-st' package
  ;; from melpa unstable.
  (display-warning 'emacs "SETUP ISSUE: form-feed-st package is not installed.")
  (defun global-form-feed-st-mode (&optional _)
    ;; Do nothing -- stub
    ))
(unless (fboundp 'global-window-tool-bar-mode)
  ;; If this triggers, make sure to install the `window-tool-bar'
  ;; package from GitHub.
  (display-warning 'emacs "SETUP ISSUE: window-tool-bar package is not installed.")
  (defun global-window-tool-bar-mode (&optional _)
    ;; Do nothing -- stub
    ))

;; This file is known to be slow, so add a bit more time here.
(cl-incf init-dir--long-load-time-warning 0.1)

;;; Package customization:

;; Intended archive sequencing:
;;
;; 1. melpa-stable
;; 2. Emacs default
;; 3. melpa (HEAD)
;;
;; Melpa (HEAD) should never auto update.
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archive-priorities
             '("melpa-stable" . 100))
(add-to-list 'package-archive-priorities
             '("melpa" . -100))
(setf package-archive-column-width 12)

;; Refreshing the list of packages takes even longer than calculating
;; the list (it involves network traffic) so run that asynchronously
;; when it won't impact user interaction.
(run-with-idle-timer
 30 nil
 (lambda () (package-refresh-contents t)))

;;; Global customizations:

(progn (bar-cursor-mode 1)
       (setf minor-mode-alist (assoc-delete-all 'bar-cursor-mode minor-mode-alist)))
(column-number-mode 1)
(context-menu-mode 1)
(cua-mode 1)
(electric-pair-mode 1)
(fido-mode 1)
(progn (global-form-feed-st-mode 1)
       (setf minor-mode-alist (assoc-delete-all 'form-feed-st-mode minor-mode-alist)))
(progn (global-subword-mode 1)
       (setf minor-mode-alist (assoc-delete-all 'subword-mode minor-mode-alist)))
(setf (default-value 'indent-tabs-mode) nil)
(recentf-mode 1)
(tooltip-mode -1)
(url-handler-mode 1)

(if window-system
    (progn (pixel-scroll-precision-mode 1)
           (setf pixel-scroll-precision-interpolate-page t))
  (xterm-mouse-mode 1)
  (when (string-match "microsoft" (shell-command-to-string "uname -r"))
    ;; Windows Console does not properly report that it supports
    ;; setSelection. It does not support other functionality.
    (setq xterm-extra-capabilities '(setSelection))

    ;; Use the default Windows browser.
    (setq browse-url-browser-function (lambda (url &rest _)
                                        (call-process "explorer.exe" nil nil nil url)))))

;; Make Emacs display similar to modern editors.
(setf frame-resize-pixelwise t
      frame-title-format "%b - Emacs"
      icon-title-format t
      scroll-conservatively most-positive-fixnum
      window-resize-pixelwise t
      x-stretch-cursor t)

(setf delete-by-moving-to-trash t
      form-feed-st-include-modes '(prog-mode text-mode special-mode)
      narrow-to-defun-include-comments t
      outline-minor-mode-use-buttons 'in-margins
      parse-sexp-lookup-properties t
      truncate-partial-width-windows nil
      use-dialog-box nil
      use-short-answers t)

;; Ignore Unity .meta files as well, they show up everywhere.
(add-to-list 'completion-ignored-extensions ".meta")

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

;; My experimental package, <http://github.com/chaosemer/window-tool-bar>
(global-window-tool-bar-mode 1)

;;; Faces:

(setf (face-background 'show-paren-match)
      (if (> (display-color-cells) 256) "light gray" "blue")
      (face-background 'show-paren-mismatch) "red"
      (face-foreground 'show-paren-mismatch) "white")

;;; Section and File level comment functionality.
;;;
;;; This allows for section and file level comments to be rendered bigger than normal text, so they
;;; stand out a bit more.
(defface section-comment-face
  '((t (:height 1.3 :inherit (font-lock-comment-face variable-pitch))))
  "Face for section level comments."
  :group 'local)
(defface file-comment-face
  '((t (:height 1.5 :weight bold :inherit (font-lock-comment-face variable-pitch))))
  "Face for file level comments."
  :group 'local)

;; Also make same adjustments for markdown.
(setf markdown-header-scaling t)

;;; Keymaps:

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
                           (when (y-or-n-p "Last frame, kill Emacs? ")
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

;;; Custom commands:

;;; DWIM <home> and <end> TODO(package)
(defun beginning-of-line-dwim (&optional _)
  "Move point to the first non-whitespace character or the beginning of line."
  (interactive "^p")

  (let ((point (point)))
    (back-to-indentation)
    (when (= point (point))
      (beginning-of-line))))

(defun end-of-line-dwim (&optional n)
  "Movie point to the last non-whitespace character or the end of line.

N: Number of lines to go forward."
  (interactive "^p")

  (let ((point (point)))
    (end-of-line n)
    (skip-chars-backward " \t")
    (when (= point (point))
      (end-of-line))))

(keymap-global-set "<home>" 'beginning-of-line-dwim)
(keymap-global-set "<end>" 'end-of-line-dwim)

;;; Recursive edits TODO(package)
(defun push-or-pop-excursion (arg)
  "Pushes or pops an excursion, depending on the prefix arg.

ARG: If nil (the default), then push an excursion.  Otherwise,
pop an excursion."
  (interactive (list current-prefix-arg))

  (if (not arg)
      (save-excursion (save-restriction (save-window-excursion (recursive-edit))))
    (when (> (recursion-depth) 0)
      (throw 'exit 'nil))))
(keymap-global-set "C-x C-p" 'push-or-pop-excursion)

;;; Sibling file navigation TODO(upstream)
(defun find-sibling-file-noselect (file)
  "Load a \"sibling\" file of FILE into a buffer and return that buffer.
The \"sibling\" file is defined by the `find-sibling-rules' variable."
  (unless find-sibling-rules
    (user-error "The `find-sibling-rules' variable has not been configured"))
  (let ((siblings (find-sibling-file-search (expand-file-name file)
                                            find-sibling-rules)))
    (cond
     ((null siblings)
      (user-error "Couldn't find any sibling files"))
     ((length= siblings 1)
      (find-file-noselect (car siblings)))
     (t
      (let ((relatives (mapcar (lambda (sibling)
                                 (file-relative-name
                                  sibling (file-name-directory file)))
                               siblings)))
        (find-file-noselect
         (completing-read (format-prompt "Find file" (car relatives))
                          relatives nil t nil nil (car relatives))))))))

(defun find-sibling-file-read-args ()
  (unless buffer-file-name
    (user-error "Not visiting a file"))
  (list buffer-file-name))

(defun my-find-sibling-file (file)
  "See `find-sibling-file'.

FILE: File to find the sibling file of."
  (interactive (find-sibling-file-read-args))
  (pop-to-buffer-same-window (find-sibling-file-noselect file)))

(defun my-find-sibling-file-other-window (file)
  "Variant of `find-sibling-file', that opens in another window.

When called interactively, find the sibling of the current
buffer's file.

FILE: File to find the sibling file of."
  (interactive (find-sibling-file-read-args))
  (switch-to-buffer-other-window (find-sibling-file-noselect file)))

(defun my-find-sibling-file-other-frame (file)
  "Variant of `find-sibling-file', that opens in another frame.

When called interactively, find the sibling of the current
buffer's file.

FILE: File to find the sibling file of."
  (interactive (find-sibling-file-read-args))
  (switch-to-buffer-other-frame (find-sibling-file-noselect file)))

(setf find-sibling-rules
      '(("\\([^/]+\\)\\.c$" "\\1.h")
        ("\\([^/]+\\)\\.cc$" "\\1.h" "\\1.hh")
        ("\\([^/]+\\)\\.cpp$" "\\1.h" "\\1.hpp")
        ("\\([^/]+\\)\\.h$" "\\1.c" "\\1.cpp" "\\1.cc")
        ("\\([^/]+\\)\\.hh$" "\\1.cc")
        ("\\([^/]+\\)\\.hpp$" "\\1.cpp")))

(keymap-global-set "C-x C-h" 'my-find-sibling-file)
(keymap-global-set "C-x 4 C-h" 'my-find-sibling-file-other-window)
(keymap-global-set "C-x 4 h" 'my-find-sibling-file-other-window)
(keymap-global-set "C-x 5 C-h" 'my-find-sibling-file-other-frame)
(keymap-global-set "C-x 5 h" 'my-find-sibling-file-other-frame)

;;; Other misc stuff TODO(package)
(defun indent-dwim (arg)
  "Try to do what a human would mean when indenting.

The prefix argument ARG, if given, indents to that column."
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
