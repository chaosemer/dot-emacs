;;; init/compilation.el --- Compilation mode customizations  -*- lexical-binding: t; -*-

;;; Code:
(defun road-runner (&rest _)
  "BEEP! BEEP!"
  (dotimes (_ 2)
    (beep) (sleep-for 0.2)))
(add-hook 'compilation-finish-functions 'road-runner)
