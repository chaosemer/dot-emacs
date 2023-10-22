;;;; C customizations.  -*- lexical-binding: t; -*-
;;;;
;;;; Also applied to other C-like languages (really anything that uses CC-mode)
(hook-mode c-mode-common-hook
  visual-line-mode
  (c-set-offset 'case-label '+)
  (c-set-offset 'innamespace 0)
  (c-set-offset 'arglist-intro '++)
  (c-set-offset 'member-init-intro '++)
  (c-set-offset 'cpp-macro 0)
  (c-set-offset 'statement-case-open '+)
  (keymap-local-set "C-c M-<right>" 'c-forward-conditional)
  (keymap-local-set "C-c M-<left>" 'c-backward-conditional)
  (keymap-local-set "C-c M-<up>" 'c-up-conditional-with-else)
  (keymap-local-set "C-c M-<down>" 'c-down-conditional)
  (when (fboundp 'cscope-bind-keys-3deep) (cscope-bind-keys-3deep))

  ;; Special comment syntax
  (font-lock-add-keywords nil '(("^\\s *\\(///.*\n?\\)" 1 'section-comment-face t)
                                ("^////.*\n?" 0 'file-comment-face t)
                                ("^//.*\\(\\s_\\|\\s.\\)\\1\\1\\1.*\n?\\(?://.*\n\\)+" 0 'section-comment-face t)
                                ("^/\\*.*\\(\\s_\\|\\s.\\)\\1\\1\\1.*\n?\\(?:.\\|\n\\)*?\\*/\n?" 0 'section-comment-face t)))
  (setf font-lock-multiline t)
  
  (setf beginning-of-defun-function 'my-c-beginning-of-defun)

  ;; ebrowse's default prefix key binding of "C-c C m -" is EXTREMELY
  ;; inconvenient.  Nothing else uses C-c C, so I'm moving it to that.
  (require 'ebrowse)
  (keymap-global-set "C-c C" ebrowse-global-map)

  ;; cc-mode defines the tab key in its map.  It shouldn't TODO(upstream)
  (keymap-unset c-mode-map "TAB")
  (keymap-unset c++-mode-map "TAB"))

(setf (default-value 'c-recognize-knr-p) nil
      (default-value 'c-recognize-paren-inits) t
      (default-value 'c-recognize-<>-arglists) t)

;; Mode hooks, such as `c-mode-commmon-hook' get called before hide-ifdef-env is set (via dirvars).
;; Hiding should be delayed until after dirvars have been processed.
(add-hook 'find-file-hook
          (lambda () (when (member major-mode '(c-mode c++-mode))
                       (hide-ifdef-mode 1)))
          t)

;; Make CC-Mode's defun finding include any function comments that
;; immediately preceede it.
(defun my-c-beginning-of-defun (&optional arg)
  "Move backward to the beginning of a defun, and any function
comment right before it."
  (ignore-errors
    (c-beginning-of-defun arg)
    (while (c-backward-single-comment))
    (re-search-forward "\\(?:\\s \\|[\n\r]\\)*" nil t)))

;; Make CScope use next-error functionality, so "C-x `" works correctly
(defun cscope-next-error (n &optional reset)
  "Advance to the next error message and visit the file where the error was.
This is the value of `next-error-function' in CScope buffers."
  (let ((buffer (get-buffer cscope-output-buffer-name)))
    (when reset
      (with-current-buffer buffer
        (goto-char (point-min))))

    (let ((do-next (> n 0)))
      (dotimes (i (abs n))
	(cscope-buffer-search t do-next)))))
(defadvice cscope-call (before my-cscope-call)
  "Wrapper to make sure `next-error-function' is set."
  (set (make-local-variable 'next-error-function) 'cscope-next-error))
(ad-activate #'cscope-call)

