;; C ---------------------------------------------------------------------------
(require-noerror 'brew)
(require 'hrule)

(hook-minor-mode c-mode-hook
  hide-ifdef-mode
  hrule-mode)
(hook-minor-mode c++-mode-hook
  hide-ifdef-mode
  hrule-mode)

(setq-default c-recognize-knr-p nil
              c-recognize-paren-inits t
              c-recognize-<>-arglists t)

;; Key bindings
(add-hook 'c-mode-hook
          (lambda ()
            (local-set-key (kbd "<f7>") 'compile)
            (when (boundp 'brew-emulator-run)
              (local-set-key (kbd "<f5>") 'brew-emulator-run))))
