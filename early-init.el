;;;; Visual changes that affect the initial frame.
;;;;
;;;; By using early-init.el, we can avoid flickering.

;; Dark mode
(load-theme 'modus-vivendi t)

;; Maximized (but not its own desktop on Mac)
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(tool-bar-mode -1)
