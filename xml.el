;; Configuration for XML files
(pushnew '("\\.xml\\'" . nxml-mode) auto-mode-alist :test #'equal)

(hook-minor-mode nxml-mode-hook

  ;;key bindings 
  (setf (local-key-binding (kbd "M-<up>")) 'nxml-backward-up-element
        (local-key-binding (kbd "M-<down>")) 'nxml-down-element))
