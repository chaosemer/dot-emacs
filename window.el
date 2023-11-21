;;;; Control the window layout. -*- lexical-binding: t; -*-
;;;;
;;;; Generally this customizes display-buffer-alist and related
;;;; options.

;; Make more switching commands do what I want.
(setf switch-to-buffer-obey-display-actions t
      switch-to-buffer-in-dedicated-window 'pop
      mouse-autoselect-window t)

;; Visual window dividers
(setf window-divider-default-places t)
(window-divider-mode 1)

;; Face remappings for side windows
(defface my-side-window
  '((t :background "#101018"))
  "Face applied to all side windows.")
(defface my-mode-line-active-side-window
  '((t :inherit mode-line-active
       :background "#202028"
       :box "#404050"))
  "Face replacement for `mode-line' in side windows.")
(defface my-mode-line-inactive-side-window
  '((t :inherit mode-line-inactive
       :background "#101018"
       :box "#202020"))
  "Face replacement for `mode-line-inactive' in side windows.")

(setf face-remapping-alist
      `((default
          (:filtered (:window window-side left) my-side-window)
          (:filtered (:window window-side bottom) my-side-window)
          default)
        (mode-line-active
         (:filtered (:window window-side left) my-mode-line-active-side-window)
         (:filtered (:window window-side bottom) my-mode-line-active-side-window)
         mode-line-active)
        (mode-line-inactive
         (:filtered (:window window-side left) my-mode-line-inactive-side-window)
         (:filtered (:window window-side bottom) my-mode-line-inactive-side-window)
         mode-line-inactive)))

(defvar my--additional-window-parameters
  (copy-tree '(window-parameters (no-other-window . t))))

(let ((bottom-windows ; Extremely transient windows
       (list "\\*Apropos\\*"
             "\\*Async Shell Command\\*"
             "\\*Backtrace\\*"
             "\\*Buffer List\\*"
             "\\*Checkdoc Status\\*"
             "\\*Completions\\*"
             "\\*Compile-Log\\*"
             "\\*Deletions\\*"
             "\\*Help\\*"
             "\\*Local Variables\\*"
             "\\*Message\\*"
             "\\*Messages\\*"
             "\\*Multiple Choice Help\\*"
             "\\*Occur\\*"
             "\\*RE-Builder\\*"
             "\\*Shell Command Output\\*"
             "\\*Warnings\\*"
             "\\*compilation\\*"
             "\\*grep\\*"
             "\\*vc-git : "
             "\\*xref\\*"))
      (left-windows ; VC prompts
       (list "\\*Open Recent - More\\*"
             "\\*vc-dir\\*"
             '(derived-mode . dired-mode))))
  (setf display-buffer-alist
        (append (mapcar (lambda (buffer)
                          `(,buffer display-buffer-in-side-window
                                    (side . bottom)
                                    ,my--additional-window-parameters))
                        bottom-windows)
                (mapcar (lambda (buffer)
                          `(,buffer display-buffer-in-side-window
                                    (side . left)
                                    ,my--additional-window-parameters))
                         left-windows))))
