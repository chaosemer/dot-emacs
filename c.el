;; C ---------------------------------------------------------------------------
(require-noerror 'brew)

(hook-minor-mode c-mode-hook hide-ifdef-mode)
(hook-minor-mode c++-mode-hook hide-ifdef-mode)


;; Key bindings
(add-hook 'c-mode-hook
          (lambda ()
            (local-set-key (kbd "<f7>") 'compile)
            (when (boundp 'brew-emulator-run)
              (local-set-key (kbd "<f5>") 'brew-emulator-run))))