;; Plain old text --------------------------------------------------------------
(require 'longlines)

(hook-minor-mode text-mode-hook
  flyspell-mode
  longlines-mode)
