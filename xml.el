;;;; Configuration for XML files

(if (>= emacs-major-version 29)
    (progn
      (add-to-list 'major-mode-remap-alist '(xml-mode . nxml-mode))
      (add-to-list 'major-mode-remap-alist '(mhtml-mode . nxml-mode))
      )
  (defalias 'xml-mode 'nxml-mode)
  (defalias 'html-mode 'nxml-mode))

(hook-mode nxml-mode-hook
  ;;key bindings 
  (setf (local-key-binding (kbd "M-<up>")) 'nxml-backward-up-element
        (local-key-binding (kbd "M-<down>")) 'nxml-down-element))
