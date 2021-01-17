;;;; Menu bar customizations  -*- lexical-binding: t; -*-
;;;;
;;;; Customizing the Emacs menu-bar. Mostly for TTYs (that's what I
;;;; used on Windows), but there's some GUI customizations here too.
;;;;
;;;; This probably should just get upstreamed.
;;;;
;;;; TODO: Make x-popup-menu properly handle mouse input (it treats a
;;;; mouse-click as "select current entry")

;; Right-click pops up the edit menu w/ some extras.
(setf (global-key-binding (kbd "<down-mouse-3>"))
      #'my-popup-right-click-menu
      (global-key-binding (kbd "<mouse-3>"))
      nil)

(defun my-popup-right-click-menu (event prefix)
  (interactive "@e\np")
  (popup-menu (my-right-keymap) event prefix))

(defun my-right-keymap ()
  "Return the top menu-bar items that should appear in a right click menu."
  (let ((old-map (current-global-map)))
    (unwind-protect
        (progn
          (use-global-map (make-sparse-keymap))
          (let  ((map (make-sparse-keymap)))
            (set-keymap-parent map (copy-keymap menu-bar-edit-menu))
            (condition-case nil
                ;; If there is no major mode or minor mode menus,
                ;; menu-bar-keymap will error internally.
                (let ((mb-map (menu-bar-keymap)))
                  (define-key map [sep-modes] '("---"))

                  ;; Since define-key adds items to the front, they
                  ;; will appear in reverse order.
                  (let ((to-add '()))
                    (map-keymap (lambda (key binding)
                                  (push (cons key binding) to-add))
                                mb-map)
                    (mapc (lambda (key-and-binding)
                            (define-key map (vector (car key-and-binding))
                              (cdr key-and-binding)))
                          to-add)))
              (wrong-type-argument))
          (keymap-canonicalize map)))
    (use-global-map old-map))))

;; Clean up the file menu
(define-key menu-bar-file-menu [new-file]
  '(menu-item "New File"
              (lambda ()
                (interactive)
                (switch-to-buffer (generate-new-buffer "untitled")))
              :enable (menu-bar-non-minibuffer-window-p)
              :help "Create a new buffer"))
(define-key menu-bar-file-menu [open-file]
  '(menu-item "Open File..." find-file
              :enable (menu-bar-non-minibuffer-window-p)
              :help "Open an existing file"))
(define-key menu-bar-file-menu [print-buffer] nil)
(define-key menu-bar-file-menu [print-region] nil)
(define-key menu-bar-file-menu [ps-print-buffer-faces] nil)
(define-key menu-bar-file-menu [ps-print-region-faces] nil)
(define-key menu-bar-file-menu [ps-print-buffer] nil)
(define-key menu-bar-file-menu [ps-print-region] nil)
