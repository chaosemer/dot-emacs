;;; init/00.emacs-patches.el --- Patches for buggy/old emacs code  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file serves two purposes:
;;
;; 1. It contains bugfixes for core Emacs functions.
;; 2. It backports new Emacs functionality to earlier versions that
;; are not yet available on Debian stable.
;;
;; Currently tested against:
;; * GNU Emacs 29.4
;; * GNU Emacs 30.1

;;; Code:

;; Actually a workaround for leotaku/elisp-check which doesn't have a
;; way to silence an intended error.
(unless (boundp 'init-dir--long-load-time-warning)
  (defvar init-dir--long-load-time-warning 0))

;; Lots of requires here slow down loading.
(defvar init-dir--long-load-time-warning)
(cl-incf init-dir--long-load-time-warning 1)

;; Waiting on Emacs support for `device-class' on other platforms.
;;
;; Currently, only x and pgtk distinguish between touchpad and mouse
;; via `device-class' so prompt the user (me!) to choose if using
;; pixel-scroll.
(unless (and (memq window-system '(x pgtk))
             (not (string-match "microsoft" (shell-command-to-string "uname -r"))))
  (defvar pixel-scroll-precision-large-scroll-height)
  (with-eval-after-load 'pixel-scroll
    (display-warning
     'emacs
     (concat
      "On this OS smooth scrolling assumes you have a touchpad.\n"
      "  Toggle if using other device: "
      (buttonize "[Mouse]"
                 (lambda (&rest _)
                   (setf pixel-scroll-precision-large-scroll-height 0)))
      " "
      (buttonize "[Touchpad]"
                 (lambda (&rest _)
                   (setf pixel-scroll-precision-large-scroll-height nil)))
      (propertize " " 'invisible t 'rear-nonsticky t)))))

;; TODO(upstream): The command `ielm-return' doesn't work well with
;; `electric-pair-mode'.  This is because while in this mode, you
;; always have a complete sexp.
(display-warning 'emacs "Fixing `ielm-return' when not at end of line")
(defvar ielm-dynamic-return)
(defvar ielm-dynamic-multiline-inputs)
(declare-function ielm-send-input "ielm" (&optional for-effect))
(declare-function ielm-pm "ielm" ())
(with-eval-after-load 'ielm
  (defun ielm-return (&optional for-effect)
    "Newline and indent, or evaluate the sexp before the prompt.
Complete sexps are evaluated; for incomplete sexps inserts a newline
and indents.  If however `ielm-dynamic-return' is nil, this always
simply inserts a newline."
    (interactive)
    (if ielm-dynamic-return
        (let ((state
               (save-excursion
                 ;; Don't do this -- I want to insert a line when not at end of line!
                 ;;(end-of-line)
                 ;; End of removal
                 (parse-partial-sexp (ielm-pm)
                                     (point)))))
          (if (and (< (car state) 1) (not (nth 3 state))
                   ;; Add this! -- I want to insert a line when not at end of line!
                   (looking-at "[ \t]*$")
                   ;; End of addition
                   )
              (ielm-send-input for-effect)
            (when (and ielm-dynamic-multiline-inputs
                       (save-excursion
                         (beginning-of-line)
                         (looking-at-p comint-prompt-regexp)))
              (save-excursion
                (goto-char (ielm-pm))
                (newline 1)))
            (newline-and-indent)))
      (newline))))
