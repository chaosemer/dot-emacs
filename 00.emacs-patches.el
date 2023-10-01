;;;; Patches for buggy/old emacs code  -*- lexical-binding: t; -*-
;;;;
;;;; This allows me to use one init file across multiple versions of Emacs.
;;;;
;;;; Currently tested against:
;;;; * GNU Emacs 28.2
;;;; * GNU Emacs 29.1

;; Fix SVG issue in 28.2.
(when (string= emacs-version "28.2")
  (display-warning 'emacs "Fixing `image-type-available-p' for 28.2.")
  (defun image-type-available-p (type)
    "Return t if image type TYPE is available.
Image types are symbols like `xbm' or `jpeg'."
    (if (eq 'svg type)
	nil
      (and (fboundp 'init-image-library)
           (init-image-library type)))))

;; TODO(upstream)
(progn
  (display-warning 'emacs "Adding setf expansions for missing things")
  (gv-define-simple-setter lookup-key define-key t))

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
  (when (lookup-key log-edit-mode-map (kbd "C-a"))
    (display-warning 'emacs "Cleaning up log-edit-mode-map")
    (setf (lookup-key log-edit-mode-map (kbd "<remap> <beginning-of-line>"))
	  (lookup-key log-edit-mode-map (kbd "C-a")))
    (setf (lookup-key log-edit-mode-map (kbd "C-a")) nil)))

;; TODO(upstream)
;;
;; The proper upstream is actually in `pixel-scroll-precision' to
;; collapse other cases to mouse. Solve via advice.
(defvar device-class--should-collapse-to-mouse nil
  "Internal variable to track if collapsing should happen")
(defvar device-class--mouse-wheel-events
  (append (list mouse-wheel-up-event mouse-wheel-down-event)
	  ;; Some variables are only added in Emacs 29.
	  (when (boundp 'mouse-wheel-up-alternate-event)
	    (list mouse-wheel-up-alternate-event))
	  (when (boundp 'mouse-wheel-down-alternate-event)
	    (list mouse-wheel-down-alternate-event))))
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

(when (and (featurep 'ns-win)
           (version<= "29.1" emacs-version))
  (display-warning 'emacs "Fixing buggy pixel-scroll-precesion")
  (define-key global-map (kbd "<remap> <pixel-scroll-precision>") 'pixel-scroll-precision--patched)
  (advice-add 'device-class :filter-return 'device-class--collapse-to-mouse))
