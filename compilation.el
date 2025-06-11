;;; init/compilation.el --- Compilation mode customizations  -*- lexical-binding: t; -*-

;;; Code:
(defun road-runner (&rest _)
  "BEEP! BEEP!"
  (dotimes (_ 2)
    (beep) (sleep-for 0.2)))
(add-hook 'compilation-finish-functions 'road-runner)

;; Accept ANSI color escape sequences since they show up in a handful
;; of tools
(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)
