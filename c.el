;; C ---------------------------------------------------------------------------
(require-noerror 'brew)
(require 'hrule)

(hook-minor-mode c-mode-hook
  hide-ifdef-mode
  hrule-mode
  
  (when (boundp 'brew-emulator-run)
    (setf (local-key-binding (kbd "<f5>") 'brew-emulator-run))))
(hook-minor-mode c++-mode-hook
  hide-ifdef-mode
  hrule-mode

  (when (boundp 'brew-emulator-run)
    (setf (local-key-binding (kbd "<f5>") 'brew-emulator-run))))

(setf (default-value 'c-recognize-knr-p) nil
      (default-value 'c-recognize-paren-inits) t
      (default-value 'c-recognize-<>-arglists) t)
