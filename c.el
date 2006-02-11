;;;; C customizations.
;;;;
;;;; Also applied to other C-like languages (really anything that uses CC-mode)
(hook-mode c-mode-common-hook
  hide-ifdef-mode
  (c-set-offset 'case-label '+)
  (c-set-offset 'innamespace 0)
  (c-set-offset 'arglist-intro '++)
  (c-set-offset 'member-init-intro '++)
  (c-set-offset 'cpp-macro 0)
  (setf (local-key-binding (kbd "C-c M-<right>")) 'c-forward-conditional
        (local-key-binding (kbd "C-c M-<left>")) 'c-backward-conditional
        (local-key-binding (kbd "C-c M-<up>")) 'c-up-conditional-with-else
        (local-key-binding (kbd "C-c M-<down>")) 'c-down-conditional)
  (font-lock-add-keywords nil '(("^\\s *\\(///.*$\\)" (1 'section-comment-face t))
                                ("^////.*$" (0 'file-comment-face t)))))
(setf (default-value 'c-recognize-knr-p) nil
      (default-value 'c-recognize-paren-inits) t
      (default-value 'c-recognize-<>-arglists) t)
