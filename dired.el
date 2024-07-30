;;; init/dired.el --- Dired mode customizations -*- lexical-binding: t; -*-

;;; Code:
(declare-function dired-icon-mode "dired-icon")
(hook-mode dired-mode-hook
  (when window-system
    (dired-icon-mode 1))
  (setq-local truncate-lines t))

;; Force use of LS emulation as it enables all my other customizations
(require 'ls-lisp)
(setf ls-lisp-use-insert-directory-program nil)

(setf
 ;; Display like most file browsers
 ls-lisp-dirs-first t
 dired-listing-switches (concat dired-listing-switches " --human-readable")

 ;; Only three columns
 ;; <permissions> <size> <filename>
 ls-lisp-verbosity '()
 ls-lisp-use-localized-time-format t
 ls-lisp-format-time-list '("" "")

 ;; Single buffer at a time
 dired-kill-when-opening-new-dired-buffer t)

;;; Keymaps:
(with-eval-after-load 'dired
  (keymap-set dired-mode-map "v" 'my-dired-vc-dir))

;;; Custom commands:
(defun my-dired-vc-dir ()
  "Show VC status for the currently displayed directory."
  (interactive)
  (unless (eq major-mode 'dired-mode)
    (error "`my-dired-vc-dir' only works in Dired mode"))
  (vc-dir (dired-current-directory)))
(with-eval-after-load 'dired
  (keymap-set dired-mode-map "<mouse-2>" #'dired-mouse-find-file))
