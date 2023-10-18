;;;; Menu bar customizations  -*- lexical-binding: t; -*-
;;;;
;;;; Customizing the Emacs menu-bar. Mostly for TTYs (that's what I
;;;; used on Windows), but there's some GUI customizations here too.
;;;;
;;;; This probably should just get upstreamed.
;;;;
;;;; TODO: Make x-popup-menu properly handle mouse input (it treats a
;;;; mouse-click as "select current entry")

;; Clean up the file menu
(keymap-set menu-bar-file-menu "<new-file>"
  '(menu-item "New File"
              (lambda ()
                (interactive)
                (switch-to-buffer (generate-new-buffer "untitled")))
              :enable (menu-bar-non-minibuffer-window-p)
              :help "Create a new buffer"))
(keymap-set menu-bar-file-menu "<open-file>"
  '(menu-item "Open File..." find-file
              :enable (menu-bar-non-minibuffer-window-p)
              :help "Open an existing file"))
