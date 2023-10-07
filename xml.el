;;;; Configuration for XML files

(add-to-list 'major-mode-remap-alist '(xml-mode . nxml-mode))
(add-to-list 'major-mode-remap-alist '(mhtml-mode . nxml-mode))

(hook-mode nxml-mode-hook
  ;;key bindings 
  (setf (local-key-binding (kbd "M-<up>")) 'nxml-backward-up-element
        (local-key-binding (kbd "M-<down>")) 'nxml-down-element))
