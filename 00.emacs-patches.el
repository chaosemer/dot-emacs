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

;; TODO(upstream)
;;
;; The proper upstream is actually in `pixel-scroll-precision' to
;; collapse other cases to mouse. Solve via advice.
(when (featurep 'ns-win)
  (with-eval-after-load 'pixel-scroll
    (defvar device-class--should-collapse-to-mouse nil
      "Internal variable to track if collapsing should happen")
    (defvar device-class--mouse-wheel-events
      (list mouse-wheel-up-event mouse-wheel-down-event
            mouse-wheel-up-alternate-event mouse-wheel-down-alternate-event))
    (defvar device-class--prev-event-timestamp 0
      "Interval variable of the previous event's timestamp.")
    (defvar device-class--prev-event-seems-like-mouse-event nil)

    (defun device-class--collapse-to-mouse (r)
      "Patch for `pixel-scroll-precision' to return mouse for pointers."
      (if (and device-class--should-collapse-to-mouse
               (eq r 'core-pointer))
	  'mouse
        r))
    (defun pixel-scroll-precision--patched (event)
      (interactive "e")
      (let* ((wheel-event (member (event-basic-type event) device-class--mouse-wheel-events))
             (line-count (event-line-count event))
             (seems-like-mouse-event (and wheel-event (/= line-count 0)))
             (timestamp (posn-timestamp (event-start event)))
             ;; collapse to mouse when the event seems like a mouse event
             (device-class--should-collapse-to-mouse
              (if (< (- timestamp device-class--prev-event-timestamp)
                     100)
		  device-class--prev-event-seems-like-mouse-event
                seems-like-mouse-event)))
        (pixel-scroll-precision event)
        (setf device-class--prev-event-timestamp timestamp
              device-class--prev-event-seems-like-mouse-event device-class--should-collapse-to-mouse)))

    (display-warning 'emacs "Fixing buggy pixel-scroll-precesion")
    (keymap-global-set "<remap> <pixel-scroll-precision>" 'pixel-scroll-precision--patched)
    (advice-add 'device-class :filter-return 'device-class--collapse-to-mouse)))

;; Fix for clicking on directory line not properly respecting
;; `dired-kill-when-opening-new-dired-buffer'. TODO(upstream, Bug#67856)
(when (version< emacs-version "30.0")
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
