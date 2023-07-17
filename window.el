;;;; Control the window layout. -*- lexical-binding: t; -*-
;;;;
;;;; Generally this customizes display-buffer-alist and related
;;;; options.

;; Make more switching commands do what I want.
(setf switch-to-buffer-obey-display-actions t
      switch-to-buffer-in-dedicated-window 'pop
      mouse-autoselect-window t)

(let ((bottom-side '(display-buffer-in-side-window
                     (side . bottom)
                     (window-parameters (no-other-window . t))))
      (left-side '(display-buffer-in-side-window
                   (side . left)
                   (window-parameters (no-other-window . t)))))
  (setf display-buffer-alist
        `(;; Extremely transient windows
          ("\\*Help\\*" ,@bottom-side)
          ("\\*grep\\*" ,@bottom-side)
          ("\\*Occur\\*" ,@bottom-side)
          ("\\*Warnings\\*" ,@bottom-side)
          ("\\*Completions\\*" ,@bottom-side)
          ("\\*Buffer List\\*" ,@bottom-side)
          ("\\*compilation\\*" ,@bottom-side)
          ("\\*Apropos\\*" ,@bottom-side)
          ("\\*Messages\\*" ,@bottom-side)
          ("\\*Backtrace\\*" ,@bottom-side)

          ;; VC prompts
          ("\\*vc-dir\\*" ,@left-side))))
