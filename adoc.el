;;; init/adoc.el --- AsciiDoc customizations -*- lexical-binding: t; -*-

;;; Declarations:
(defvar adoc-mode-map)

;;; Keymaps:
(with-eval-after-load 'adoc-mode
  ;; Use normal navigation always
  (keymap-unset adoc-mode-map "M-<left>")
  (keymap-unset adoc-mode-map "M-<right>"))
