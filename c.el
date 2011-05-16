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
  (c-set-offset 'statement-case-open '+)
  (setf (local-key-binding (kbd "C-c M-<right>")) 'c-forward-conditional
        (local-key-binding (kbd "C-c M-<left>")) 'c-backward-conditional
        (local-key-binding (kbd "C-c M-<up>")) 'c-up-conditional-with-else
        (local-key-binding (kbd "C-c M-<down>")) 'c-down-conditional)
  (when (featurep 'cscope) (cscope-bind-keys-3deep))

  ;; Special comment syntax
  (font-lock-add-keywords nil '(("^\\s *\\(///.*\n?\\)" 1 'section-comment-face t)
                                ("^////.*\n?" 0 'file-comment-face t)
								("^//.*\\(\\s_\\|\\s.\\)\\1\\1\\1.*\n?\\(?://.*\n\\)+" 0 'section-comment-face t)
								("^/\\*.*\\(\\s_\\|\\s.\\)\\1\\1\\1.*\n?\\(?:.\\|\n\\)*?\\*/\n?" 0 'section-comment-face t)))
  (setf font-lock-multiline t)
  (add-hook 'font-lock-extend-region-functions 'my-c-section-comment-extend-region t)
  
  (setf beginning-of-defun-function 'my-c-beginning-of-defun))

(defun my-c-section-comment-extend-region ()
  "Function to extend the font lock region to support section comments."
  
  (let (new-beg new-end)
  	;; back up to the beginning of this comment block
  	(goto-char font-lock-beg)
	(unless (bolp) (forward-line 0))

  	(while (and (not (bobp)) (looking-at "//"))
  	  (forward-line -1))
  	(setf new-beg (point))

	;;(setf new-end font-lock-end)
  	;; advance to the end of this comment block
  	(goto-char font-lock-end)
	(unless (bolp) (forward-line 1))  	
  	(while (and (not (eobp)) (looking-at "//"))
  	  (forward-line 1))
  	(setf new-end (point))

  	(prog1 (or (/= new-beg font-lock-beg)
  			   (/= new-end font-lock-end))
  	  (setf font-lock-beg new-beg font-lock-end new-end))))

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

;; ebrowse's default prefix key binding of "C-c C m -" is EXTREMELY inconvenient.  Nothing else uses
;; C-c C, so I'm moving it to that.
(setf (global-key-binding (kbd "C-c C")) ebrowse-global-map)

;; cc-mode defines the tab key in its map.  It shouldn't
(require 'cc-mode)
(define-key c-mode-map (kbd "TAB") nil)
(define-key c++-mode-map (kbd "TAB") nil)

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

