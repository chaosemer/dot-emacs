;;;; Visual changes that affect the initial frame.
;;;;
;;;; By using early-init.el, we can avoid flickering.

;; Dark mode
(setf modus-themes-headings
      '((1 . (variable-pitch (height 1.5)))
        (2 . (variable-pitch (height 1.3)))
        (3 . (variable-pitch (height 1.1)))
        (t . (t))))
(load-theme 'modus-vivendi t)

;; Maximized (but not its own desktop on Mac)
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(tool-bar-mode -1)
