;;;; Patches for buggy/old emacs code  -*- lexical-binding: t; -*-
;;;;
;;;; This allows me to use one init file across multiple versions of Emacs.
;;;;
;;;; Currently tested against:
;;;; * GNU Emacs 29.1

;; Disallow navigating to the minibuffer
(unless (eq (plist-get minibuffer-prompt-properties 'cursor-intangible) t)
  (display-warning 'emacs "Disallowing navigation into the minibuffer prompt")
  (setf minibuffer-prompt-properties
	(plist-put minibuffer-prompt-properties 'cursor-intangible t))
  (add-hook 'minibuffer-setup-hook 'cursor-intangible-mode))

;; Fix buggy regexp in Emacs TODO(upstream)
;;   incorrect-regexp "^ *\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^ :(\t\n][^:(\t\n]*\\)(\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)?) ?: \\(?:see declaration\\|\\(?:warnin\\(g\\)\\|[a-z ]+\\) C[0-9]+:\\)"
(with-eval-after-load 'compile
  (let ((correct-regexp "^ *\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) ?: \\(?:see declaration\\|\\(?:warnin\\(g\\)\\|[a-z ]+\\) [A-Z][0-9]+:\\)"))
    (unless (equal (nth 1 (assoc 'msft compilation-error-regexp-alist-alist)) correct-regexp)
      (display-warning 'emacs "Fixing buggy Microsoft regexp")
      (setf (nth 1 (assoc 'msft compilation-error-regexp-alist-alist)) correct-regexp))))

;; Making C-a in log-edit-mode not be there TODO(upstream)
(with-eval-after-load 'log-edit
  (when (keymap-lookup log-edit-mode-map "C-a")
    (display-warning 'emacs "Cleaning up log-edit-mode-map")
    (keymap-set log-edit-mode-map "<remap> <beginning-of-line>"
                (keymap-lookup log-edit-mode-map "C-a"))
    (keymap-unset log-edit-mode-map "C-a")))

;; TODO(upstream)
;;
;; The proper upstream is actually in `pixel-scroll-precision' to
;; collapse other cases to mouse. Solve via advice.
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

(when (featurep 'ns-win)
  (display-warning 'emacs "Fixing buggy pixel-scroll-precesion")
  (keymap-global-set "<remap> <pixel-scroll-precision>" 'pixel-scroll-precision--patched)
  (advice-add 'device-class :filter-return 'device-class--collapse-to-mouse))
