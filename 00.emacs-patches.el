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
;; * GNU Emacs 29.1
;; * GNU Emacs 29.2
;; * GNU Emacs 29.3

;;; Code:

;; Actually a workaround for leotaku/elisp-check which doesn't have a
;; way to silence an intended error.
(unless (boundp 'init-dir--long-load-time-warning)
  (defvar init-dir--long-load-time-warning 0))

;; Lots of requires here slow down loading.
(cl-incf init-dir--long-load-time-warning 1)

;; Fix buggy regexp in Emacs TODO(upstream)
;;   incorrect-regexp "^ *\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^ :(\t\n][^:(\t\n]*\\)(\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)?) ?: \\(?:see declaration\\|\\(?:warnin\\(g\\)\\|[a-z ]+\\) C[0-9]+:\\)"
(require 'compile)
(let ((correct-regexp "^ *\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) ?: \\(?:see declaration\\|\\(?:warnin\\(g\\)\\|[a-z ]+\\) [A-Z][0-9]+:\\)"))
  (unless (equal (nth 1 (assoc 'msft compilation-error-regexp-alist-alist)) correct-regexp)
    (display-warning 'emacs "Fixing buggy Microsoft regexp")
    (setf (nth 1 (assoc 'msft compilation-error-regexp-alist-alist)) correct-regexp)))

;; Making C-a in log-edit-mode not be there TODO(Bug#67851, fixed in 30.1)
(require 'log-edit)
(when (keymap-lookup log-edit-mode-map "C-a")
  (display-warning 'emacs "Cleaning up log-edit-mode-map")
  (keymap-set log-edit-mode-map "<remap> <beginning-of-line>"
              (keymap-lookup log-edit-mode-map "C-a"))
  (keymap-unset log-edit-mode-map "C-a"))

;; Fix for clicking on directory line not properly respecting
;; `dired-kill-when-opening-new-dired-buffer'. TODO(Bug#67856, fixed in 29.2)
(when (version< emacs-version "29.2")
  (require 'dired)
  (display-warning 'emacs "Fixing buggy behavior in dired--make-directory-clickable")
  (defun dired--make-directory-clickable ()
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              (if (memq system-type '(windows-nt ms-dos))
                  "^  \\([a-zA-Z]:/\\|//\\)"
                "^  /")
              nil t 1)
        (let ((bound (line-end-position))
              (segment-start (point))
              (inhibit-read-only t)
              (dir (substring (match-string 0) 2)))
          (while (search-forward "/" bound t 1)
            (setq dir (concat dir (buffer-substring segment-start (point))))
            (add-text-properties
             segment-start (1- (point))
             `( mouse-face highlight
                help-echo "mouse-1: goto this directory"
                keymap ,(let* ((current-dir dir)
                               (click (lambda ()
                                        (interactive)
                                        (cond
                                          ((assoc current-dir dired-subdir-alist)
                                           (dired-goto-subdir current-dir))
                                          ;; Defensive programming -- is this case actually hit?
                                          ((insert-directory-wildcard-in-dir-p current-dir)
                                           (dired current-dir))
                                          (t
                                           (dired--find-possibly-alternative-file current-dir))))))
                          (define-keymap
                            "<mouse-2>" click
                            "<follow-link>" 'mouse-face
                            "RET" click))))
            (setq segment-start (point))))))))

;; Add support for Windows' Alt-F4 to close window key binding. TODO(upstream)
(when (and (eq window-system 'w32)
           (null (keymap-global-lookup "M-<f4>")))
  (display-warning 'emacs "Adding keybinding for M-<f4>.")
  ;; Real fix is in `handle-delete-frame'.  For now, just duplicate
  ;; code here.
  (defun my-handle-delete-frame ()
    (interactive)
    (let ((frame (window-frame)))
      (if (catch 'other-frame
            (dolist (frame-1 (frame-list))
              ;; A valid "other" frame is visible, has its `delete-before'
              ;; parameter unset and is not a child frame.
              (when (and (not (eq frame-1 frame))
                         (frame-visible-p frame-1)
                         (not (frame-parent frame-1))
                         (not (frame-parameter frame-1 'delete-before)))
                (throw 'other-frame t))))
	  (delete-frame frame t)
        ;; Gildea@x.org says it is ok to ask questions before terminating.
        (save-buffers-kill-emacs))))
  (keymap-global-set "M-<f4>" #'my-handle-delete-frame))


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
