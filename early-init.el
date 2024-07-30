;;; init/early-init.el --- Visual changes that affect the initial frame  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file should only be loaded during the early init and not
;; during regular init.  This is only for fundamental visual changes
;; that would otherwise cause Emacs to flicker during startup.

;;; Code:
;; `early-init-file' is set after the early init process, which serves
;; as a clear flag.
(defvar modus-themes-headings)
(unless early-init-file
  (setf modus-themes-headings
        '((1 . (variable-pitch (height 1.5)))
          (2 . (variable-pitch (height 1.3)))
          (3 . (variable-pitch (height 1.1)))
          (t . (t))))

  ;; Dark mode
  (load-theme 'modus-vivendi t)

  ;; Maximized (but not its own desktop on Mac)
  (add-to-list 'initial-frame-alist '(fullscreen . maximized))

  ;; No tool-bar (the global tool bar isn't useful)
  (add-to-list 'default-frame-alist '(tool-bar-lines . 0)))
