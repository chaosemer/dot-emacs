;; Configuration for XML files
(require 'nxml-mode)

(pushnew '("\\.xml\\'" . nxml-mode) auto-mode-alist :test #'equal)

(hook-minor-mode nxml-mode-hook
  hrule-mode)

;; key bindings
(add-hook 'nxml-mode-hook
          (lambda ()
            (setf (local-key-binding (kbd "M-<up>")) 'nxml-backward-up-element
                  (local-key-binding (kbd "M-<down>")) 'nxml-down-element)))
