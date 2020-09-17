;;;; Menu bar customizations
;;;;
;;;; Customizing the Emacs menu-bar. Mostly for TTYs (that's what I
;;;; used on Windows), but there's some GUI customizations here too.
;;;;
;;;; This probably should just get upstreamed.
;;;;
;;;; TODO: Make x-popup-menu properly handle mouse input (it treats a
;;;; mouse-click as "select current entry")

;; Right-click pops up the edit menu.
(setf (global-key-binding (kbd "<mouse-3>"))
      (lambda (event prefix)
        (interactive "@e\np")
        (popup-menu menu-bar-edit-menu event prefix)))

;; On TTYs, clicking the menu bar pops up the TTY menu - WIP
(defun my-menu-bar-open (event)
  (interactive "e")
  (let ((x (car (posn-x-y (event-start event)))))
    (let ((tty-menu--initial-menu-x (my-menu-bar-discretize-x x)))
      (if (>= emacs-major-version 27)
          (menu-bar-open nil 0)
        (menu-bar-open)))))

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

(setf (global-key-binding (kbd "<menu-bar> <mouse-1>"))
      'my-menu-bar-open)

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

;;; Major mode list in tools menu
(defun list-major-modes ()
  "Returns a list of major modes"

  (flet ((alist-mode (val)
                     (if (consp (cdr val)) (cadr val) (cdr val))))
    (let ((modes (nconc (mapcar 'alist-mode auto-mode-alist)
                        (mapcar 'alist-mode interpreter-mode-alist)
                        (mapcar 'alist-mode magic-mode-alist))))
      ;; the alists allow nil and t to mean special things
      (setf modes (delete-if (lambda (mode) (memq mode '(t nil))) modes))

      ;; shouldn't display modes that require special text setup
      (setf modes (delete-if (lambda (mode) (eq (get mode 'mode-class) 'special)) modes))

      ;; return list sorted
      (remove-duplicates (sort modes 'string-lessp)))))

(defun menu-major-modes ()
  (flet ((doc-summary (fn)
                      (let ((doc (documentation fn)))
                        (substring doc 0 (position ?\n doc)))))
    (let ((menu (make-sparse-keymap "Major Modes"))
          (major-mode-list (list-major-modes)))
      ;; Remove modes we don't want to normally list.
      (setq major-mode-list
            (delete-if (lambda (elt) (or (memq elt important-major-modes)
                                         (memq elt unimportant-major-modes)))
                       major-mode-list))

      (flet ((make-menu-item (mode)
                             (ignore-errors
                               `(,mode menu-item ,(symbol-name mode) ,mode
                                       :button (:toggle . (eq major-mode ',mode))
                                       :help ,(doc-summary mode)))))
        (setq menu (nconc menu
                          (mapcar 'make-menu-item important-major-modes)
                          (list '(sep1 menu-item "---"))
                          (mapcar 'make-menu-item major-mode-list))))
    menu)))

(defvar important-major-modes
  '(fundamental-mode c-mode c++-mode emacs-lisp-mode)
  "A list of important major modes to display seperately.

These modes will be displayed in a separate area in the order
listed.")
(setf unimportant-major-modes
  '(conf-javaprop-mode conf-unix-mode conf-windows-mode
    conf-mode-maybe conf-space-mode conf-xdefaults-mode
    conf-colon-mode conf-ppd-mode
    ebrowse-tree-mode image-mode-maybe mail-mode))
(defvar other-major-modes
  '()
  "A list of major modes that are not automatically detected by `list-major-modes'.")


(define-key menu-bar-options-menu [sep] '(menu-item "---"))
(define-key menu-bar-options-menu [major-modes-list]
  '(menu-item "Major Mode" nil :filter (lambda (menu) (menu-major-modes))))
