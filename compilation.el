;;;; Compilation mode customizations
(hook-mode compilation-mode-hook
  next-error-follow-minor-mode)
(hook-mode occur-mode-hook
  next-error-follow-minor-mode)

(defun road-runner (&rest args)
  "BEEP! BEEP!"
  (dotimes (i 2)
    (beep) (sleep-for 0.2)))
(add-hook 'compilation-finish-functions 'road-runner)
