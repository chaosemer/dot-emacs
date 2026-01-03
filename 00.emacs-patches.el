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
;; * GNU Emacs 30.1
;; * GNU Emacs 30.2

;;; Declarations:
(declare-function ibuffer-do-sort-by-alphabetic "ibuf-ext")
(declare-function ibuffer-switch-to-saved-filter-groups "ibuf-ext")
(declare-function ibuffer-toggle-filter-group "ibuf-ext")
(declare-function ielm-pm "ielm")
(declare-function ielm-send-input "ielm")
(defvar comint-prompt-regexp)
(defvar ibuffer-hidden-filter-groups)
(defvar ibuffer-hidden-filter-groups)
(defvar ibuffer-mode-filter-group-map)
(defvar ibuffer-saved-filter-groups)
(defvar ibuffer-show-empty-filter-groups)
(defvar ielm-dynamic-multiline-inputs)
(defvar ielm-dynamic-return)
(defvar init-dir--long-load-time-warning)
(defvar pixel-scroll-precision-large-scroll-height)

;;; Code:

;; Actually a workaround for leotaku/elisp-check which doesn't have a
;; way to silence an intended error.
(unless (boundp 'init-dir--long-load-time-warning)
  (defvar init-dir--long-load-time-warning 0))

;; Lots of requires here slow down loading.
(cl-incf init-dir--long-load-time-warning 1)

;; Waiting on Emacs support for `device-class' on other platforms.
;;
;; Currently, only x and pgtk distinguish between touchpad and mouse
;; via `device-class' so prompt the user (me!) to choose if using
;; pixel-scroll.
(unless (and (memq window-system '(x pgtk))
             (not (string-match "microsoft" (shell-command-to-string "uname -r"))))
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

;; TODO(upstreaming in bug#80123): The command `ielm-return' doesn't work well with
;; `electric-pair-mode'.  This is because while in this mode, you
;; always have a complete sexp.
(display-warning 'emacs "Fixing `ielm-return' when not at end of line")
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

;; https://www2.lib.uchicago.edu/keith/emacs/ recommends alternaties
;; to `list-buffers'.
(display-warning
 'emacs
 (concat
  "You are trying out alternative bindings for C-x C-b.\n"
  "  Toggle the binding: "
  (buttonize "[list-buffers]"
             (lambda (&rest _)
               (keymap-global-set "C-x C-b" #'list-buffers)))
  " "
  (buttonize "[bs-show]"
             (lambda (&rest _)
               (keymap-global-set "C-x C-b" #'bs-show)))
  " "
  (buttonize "[ibuffer]"
             (lambda (&rest _)
               (keymap-global-set "C-x C-b" #'ibuffer)))
  (propertize " " 'invisible t 'rear-nonsticky t)))
(keymap-global-set "C-x C-b" #'ibuffer)
(with-eval-after-load 'ibuffer
  (keymap-set ibuffer-mode-filter-group-map
              "<mouse-1>" #'ibuffer-toggle-filter-group))
(setf ibuffer-show-empty-filter-groups nil
      ibuffer-saved-filter-groups '(("Home"
                                     ("Files" (visiting-file))
                                     ("Starred" (starred-name)))))
(add-hook 'ibuffer-mode-hook
          (defun my-ibuffer-hook ()
            (ibuffer-switch-to-saved-filter-groups "Home")
            (ibuffer-do-sort-by-alphabetic)
            (setf ibuffer-hidden-filter-groups '("Starred"))))
