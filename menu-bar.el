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

;; Upstreaming, make clicking on the menu bar pop up the right TTY menu.
(if emacs-repository-version
    (setf (global-key-binding (kbd "<menu-bar> <mouse-1>"))
          'menu-bar-open-mouse)
  (setf (global-key-binding (kbd "<menu-bar> <mouse-1>"))
        'my-menu-bar-open)
(defun my-popup-right-click-menu (event prefix)
  (interactive "@e\np")
  (popup-menu (my-right-keymap) event prefix))

  (defun my-menu-bar-open (event)
    (interactive "e")
    (let ((x (car (posn-x-y (event-start event)))))
      (let ((tty-menu--initial-menu-x (my-menu-bar-discretize-x x)))
        (if (>= emacs-major-version 27)
            (menu-bar-open nil 0)
          (menu-bar-open)))))
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

  (defun my-menu-bar-discretize-x (x)
    "Given X, convert to the left-most position for the associated
menu entry. Returns nil if no such value exists."
    (let ((menu-x 0))
      (dolist (string (menu-bar-item-strings))
        (let ((next-menu-x (+ menu-x (length string) 1)))
          (when (< x next-menu-x)
            (return menu-x))
          (setf menu-x next-menu-x)))))

  (defun menu-bar-item-strings ()
    "Return a list of strings for the active menu-bar."
    (mapcar (lambda (item)
              (cond
               ;; Simple menu item
               ((stringp (car item)) (car item))
               ;; Extended menu item
               ((eq (car item) 'menu-item) (cadr item))))
            (menu-bar-items)))

  (defun menu-bar-items ()
    "Return a list of active menu-bar keymap entries."
    (let ((items '())
          (final-items '()))
      (dolist (keymap (reverse (current-active-maps)))
        (let ((item (lookup-key keymap (kbd "<menu-bar>"))))
          ;; The menu bar contains only the keymaps bound to
          ;; <menu-bar>. Other entries are for mouse input.
          (when (keymapp item)
            (map-keymap
             (lambda (e i)
               (when (or
                      ;; Simple menu item
                      (and (listp i) (stringp (car i)))
                      ;; Extended menu item
                      (and (listp i) (eq (car i) 'menu-item)))
                 (if (member e menu-bar-final-items)
                     (push i final-items)
                   (push i items))))
             item))))
      (concatenate 'list (reverse items) final-items)))
  )
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
