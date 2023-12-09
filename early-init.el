;;;; Visual changes that affect the initial frame.  -*- lexical-binding: t; -*-

;; This file should only be loaded during the early init and not
;; during regular init. `early-init-file' is set after the early init
;; process, which serves as a clear flag.
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
