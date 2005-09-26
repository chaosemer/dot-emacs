;; C ---------------------------------------------------------------------------
(hook-minor-mode c-mode-common-hook
  hide-ifdef-mode
  (c-set-offset 'case-label '+)
  (setf comment-start "/* "
        comment-end " */"))

(setf (default-value 'c-recognize-knr-p) nil
      (default-value 'c-recognize-paren-inits) t
      (default-value 'c-recognize-<>-arglists) t)
