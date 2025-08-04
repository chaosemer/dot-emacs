;;; init/markdown.el --- Markdown customizations -*- lexical-binding: t; -*-

;;; Custom commands

;; Patch markdown mode
(with-eval-after-load 'markdown-mode
  (display-warning 'emacs "Removing - and + as bullets in Markdown")
  (defmacro markdown-rx (&rest regexps)
    `(rx-let ((newline "\n")
              ;; Note: #405 not consider markdown-list-indent-width however this is never used
              (indent (or (repeat 4 " ") "\t"))
              (block-end (and (or (one-or-more (zero-or-more blank) "\n") line-end)))
              (numeral (and (one-or-more (any "0-9#")) "."))
              (bullet (any "*:"))
              (list-marker (or (and (one-or-more (any "0-9#")) ".")
                               (any "*:")))
              (checkbox (seq "[" (any " xX") "]")))
       (rx ,@regexps)))

  (defconst markdown-regex-list
    (markdown-rx line-start
                 ;; 1. Leading whitespace
                 (group (* blank))
                 ;; 2. List marker: a numeral, bullet, or colon
                 (group list-marker)
                 ;; 3. Trailing whitespace
                 (group (+ blank))
                 ;; 4. Optional checkbox for GFM task list items
                 (opt (group (and checkbox (* blank)))))
    "Regular expression for matching list items."))
