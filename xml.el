;; Configuration for XML files
(require 'nxml-mode)

(hook-minor-mode nxml-mode-hook
  hrule-mode)

;; key bindings
(add-hook 'nxml-mode-hook
          (lambda ()
            (setf (local-key-binding (kbd "M-<up>")) 'nxml-backward-up-element
                  (local-key-binding (kbd "M-<down>")) 'nxml-down-element)))
