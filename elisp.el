;; Emacs Lisp ------------------------------------------------------------------
(defalias 'elisp-repl #'ielm)

;(defadvice ielm-send-input (after ielm-read-only first nil activate)
;  (message "test")
;  (save-excursion
;    (let* ((min (point-min))
;           (max (point-max)))
;      (add-text-properties min max '(read-only t rear-nonsticky t)))))