;;;; Hexl (hex editor) customizations.
(require 'hexl)

(defun hexl-insert-nybble (ch arg)
  "Insert nybble for character ch arg times."
  (when (not (or (<= ?0 last-command-event ?9)
                 (<= ?a last-command-event ?f)
                 (<= ?A last-command-event ?F)))
    (error "Invalid nybble %c" last-command-event))

  (dotimes (_ arg)
    (let* ((address (hexl-current-address t))
           (hex-position (hexl-address-to-marker address))
           (byte-str (buffer-substring hex-position (+ hex-position 2)))

           ;; nybble index
           (index (if (= (point) (1+ hex-position))
                       1 0)))
      (setf (aref byte-str index) last-command-event)

      (save-excursion (hexl-insert-hex-string byte-str 1))
      (goto-char (+ hex-position index 1)))))

(defun hexl-my-self-insert-command (arg)
  "Replacement for `hexl-self-insert-command'."
  (interactive "p")
  (if (< (current-column) (hexl-ascii-start-column))
      (hexl-insert-nybble last-command-event arg)
    (hexl-insert-multibyte-char last-command-event arg)))

(define-key hexl-mode-map (kbd "<remap> <self-insert-command>")
  'hexl-my-self-insert-command)
(define-key hexl-mode-map (kbd "\C-m") 'hexl-my-self-insert-command)
