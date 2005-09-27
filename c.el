;; C ---------------------------------------------------------------------------
(hook-minor-mode c-mode-common-hook
  hide-ifdef-mode
  (c-set-offset 'case-label '+)
  (setf comment-start "/* "
        comment-end " */"
        (local-key-binding (kbd "C-c M-<right>")) 'c-forward-conditional
        (local-key-binding (kbd "C-c M-<left>")) 'c-backward-conditional
        (local-key-binding (kbd "C-c M-<up>")) 'c-up-conditional-with-else
        (local-key-binding (kbd "C-c M-<down>")) 'c-down-conditional))

(setf (default-value 'c-recognize-knr-p) nil
      (default-value 'c-recognize-paren-inits) t
      (default-value 'c-recognize-<>-arglists) t)
