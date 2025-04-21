;;; init/c.el --- C based language customizations  -*- lexical-binding: t; -*-

;;; Declarations:
(declare-function c-backward-single-comment "cc-engine")
(declare-function c-beginning-of-defun "cc-cmds")
(declare-function cscope-buffer-search "cscope")
(declare-function cscope-call "cscope")
(defvar c-mode-base-map)
(defvar c-mode-common-hook)
(defvar cscope-output-buffer-name)
(defvar ebrowse-global-prefix-key)

;;; Code:
(add-hook 'c-mode-common-hook
          (defun my-c-mode-common-hook ()
            (visual-line-mode)
            (c-set-offset 'case-label '+)
            (c-set-offset 'innamespace 0)
            (c-set-offset 'arglist-intro '++)
            (c-set-offset 'member-init-intro '++)
            (c-set-offset 'cpp-macro 0)
            (c-set-offset 'statement-case-open '+)
            (when (fboundp 'cscope-bind-keys-3deep) (cscope-bind-keys-3deep))

            ;; Special comment syntax
            (font-lock-add-keywords nil '(("^\\s *\\(///.*\n?\\)" 1 'section-comment-face t)
                                          ("^////.*\n?" 0 'file-comment-face t)
                                          ("^//.*\\(\\s_\\|\\s.\\)\\1\\1\\1.*\n?\\(?://.*\n\\)+" 0 'section-comment-face t)
                                          ("^/\\*.*\\(\\s_\\|\\s.\\)\\1\\1\\1.*\n?\\(?:.\\|\n\\)*?\\*/\n?" 0 'section-comment-face t)))
            (setf font-lock-multiline t)

            (setf beginning-of-defun-function 'my-c-beginning-of-defun)

            (require 'ebrowse)))

(setf (default-value 'c-recognize-knr-p) nil
      (default-value 'c-recognize-paren-inits) t
      (default-value 'c-recognize-<>-arglists) t)

;; Mode hooks, such as `c-mode-commmon-hook' get called before hide-ifdef-env is set (via dirvars).
;; Hiding should be delayed until after dirvars have been processed.
(add-hook 'find-file-hook
          (lambda () (when (member major-mode '(c-mode c++-mode))
                       (hide-ifdef-mode 1)))
          t)

;;; Keymaps:
(with-eval-after-load 'cc-mode
  (keymap-set c-mode-base-map "C-c M-<right>" 'c-forward-conditional)
  (keymap-set c-mode-base-map "C-c M-<left>" 'c-backward-conditional)
  (keymap-set c-mode-base-map "C-c M-<up>" 'c-up-conditional-with-else)
  (keymap-set c-mode-base-map "C-c M-<down>" 'c-down-conditional))

;; ebrowse's default prefix key binding of "C-c C m -" is EXTREMELY
;; inconvenient.  Nothing else uses C-c C, so I'm moving it to that.
(setf ebrowse-global-prefix-key (kbd "C-c C"))

;;; Custom commands:

;; Make CC-Mode's defun finding include any function comments that
;; immediately preceede it.
(defun my-c-beginning-of-defun (&optional arg)
  "Move backward to the beginning of a defun, including comments.

Unlike `c-beginning-of-defun', this also includes the comment
block in front of it.

ARG: Number of defuns to move, as in `c-beginning-of-defun'."
  (ignore-errors
    (c-beginning-of-defun arg)
    (while (c-backward-single-comment))
    (re-search-forward "\\(?:\\s \\|[\n\r]\\)*" nil t)))

;; Make CScope use next-error functionality, so "C-x `" works correctly
(defun cscope-next-error (n &optional reset)
  "Advance to the next error message and visit the file where the error was.
This is the value of `next-error-function' in CScope buffers.

N: Number of errors to move forward.
RESET: If non-nil, start from the beginning."
  (let ((buffer (get-buffer cscope-output-buffer-name)))
    (when reset
      (with-current-buffer buffer
        (goto-char (point-min))))

    (let ((do-next (> n 0)))
      (dotimes (_ (abs n))
	(cscope-buffer-search t do-next)))))
(define-advice cscope-call (:before () my-cscope-call)
  "Wrapper to make sure `next-error-function' is set."
  (set (make-local-variable 'next-error-function) 'cscope-next-error))
