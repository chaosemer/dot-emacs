;;; init/compilation.el --- Compilation mode customizations  -*- lexical-binding: t; -*-

;;; Code:
(hook-mode compilation-mode-hook
  next-error-follow-minor-mode)
(hook-mode occur-mode-hook
  next-error-follow-minor-mode)

(defun road-runner (&rest _)
  "BEEP! BEEP!"
  (dotimes (_ 2)
    (beep) (sleep-for 0.2)))
(add-hook 'compilation-finish-functions 'road-runner)
