;;;; Configuration for XML files
(defalias 'xml-mode 'nxml-mode)

(hook-mode nxml-mode-hook
  ;;key bindings 
  (setf (local-key-binding (kbd "M-<up>")) 'nxml-backward-up-element
        (local-key-binding (kbd "M-<down>")) 'nxml-down-element))
