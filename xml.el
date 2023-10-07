;;;; Configuration for XML files

(add-to-list 'major-mode-remap-alist '(xml-mode . nxml-mode))
(add-to-list 'major-mode-remap-alist '(mhtml-mode . nxml-mode))

(hook-mode nxml-mode-hook
  ;; key bindings
  (keymap-local-set "M-<up>" 'nxml-backward-up-element)
  (keymap-local-set "M-<down>" 'nxml-down-element))
