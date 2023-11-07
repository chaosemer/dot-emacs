;;;; Dired mode customizations. -*- lexical-binding: t; -*-

(hook-mode dired-mode-hook
  (when window-system
    (dired-icon-mode 1))
  (setq-local truncate-lines t))

;; Force use of LS emulation as it enables all my other customizations
(require 'ls-lisp)
(setf ls-lisp-use-insert-directory-program nil)


(setf
 ;; Like most file browsers
 ls-lisp-dirs-first t
 dired-listing-switches (concat dired-listing-switches " --human-readable")

 ;; Extremely low verbosity:
 ;; <permissions> <size> <filename>
 ls-lisp-verbosity '()
 ls-lisp-use-localized-time-format t
 ls-lisp-format-time-list '("" ""))
