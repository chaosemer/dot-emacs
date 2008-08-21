;;;; C customizations.
;;;;
;;;; Also applied to other C-like languages (really anything that uses CC-mode)
(require 'ebrowse)
(load "xcscope" t)

(hook-mode c-mode-common-hook
  (c-set-offset 'case-label '+)
  (c-set-offset 'innamespace 0)
  (c-set-offset 'arglist-intro '++)
  (c-set-offset 'member-init-intro '++)
  (c-set-offset 'cpp-macro 0)
  (setf (local-key-binding (kbd "C-c M-<right>")) 'c-forward-conditional
        (local-key-binding (kbd "C-c M-<left>")) 'c-backward-conditional
        (local-key-binding (kbd "C-c M-<up>")) 'c-up-conditional-with-else
        (local-key-binding (kbd "C-c M-<down>")) 'c-down-conditional)
  (when (featurep 'cscope) (cscope-bind-keys-3deep))
  (font-lock-add-keywords nil '(("^\\s *\\(///.*$\\)" (1 'section-comment-face t))
                                ("^////.*$" (0 'file-comment-face t))))
  (setf beginning-of-defun-function 'my-c-beginning-of-defun))

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

  (c-beginning-of-defun arg)
  (while (c-backward-single-comment))
  (re-search-forward "\\(?:\\s \\|[\n\r]\\)*" nil t))

;; ebrowse's default prefix key binding of "C-c C m -" is EXTREMELY inconvenient.  Nothing else uses
;; C-c C, so I'm moving it to that.
(setf (global-key-binding (kbd "C-c C")) ebrowse-global-map)

;; cc-mode defines the tab key in its map.  It shouldn't
(require 'cc-mode)
(define-key c-mode-map (kbd "TAB") nil)
(define-key c++-mode-map (kbd "TAB") nil)
