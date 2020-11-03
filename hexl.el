;;;; Hexl (hex editor) customizations.  -*- lexical-binding: t; -*-
(require 'hexl)

(hook-mode hexl-mode-hook
  (hexl-follow-line)
  (hexl-activate-ruler)
  (turn-on-eldoc-mode)
  (setf truncate-lines t))

;; TODO(upstream)
(defun hexl-insert-nybble (ch arg)
  "Insert nybble for character ch arg times."
  (when (not (or (<= ?0 last-command-event ?9)
                 (<= ?a last-command-event ?f)
                 (<= ?A last-command-event ?F)))
    (error "Invalid nybble %c" last-command-event))

  (dotimes (_ arg)
    (let* ((hex-position (hexl-address-to-marker (hexl-current-address)))
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

(defun hexl-my-backward-char (arg)
  "Move to left ARG bytes (right if ARG negative) in Hexl mode."
  (interactive "p")
  (hexl-my-forward-char (- arg)))


(defun hexl-my-forward-char (arg)
  "Move to right ARG bytes (left if ARG negative) in Hexl mode."
  (interactive "p")
  (let* ((hex-position (hexl-address-to-marker (hexl-current-address)))
         (index (if (= (point) (1+ hex-position))
                    1 0)))
    (hexl-goto-address (+ (hexl-current-address)
                          arg
                          (if (and (< arg 0) (/= index 0))
                              1 0)))))


(define-key hexl-mode-map (kbd "<remap> <self-insert-command>")
  'hexl-my-self-insert-command)
(define-key hexl-mode-map (kbd "\C-m") 'hexl-my-self-insert-command)
(define-key hexl-mode-map (kbd "<left>") 'hexl-my-backward-char)
(define-key hexl-mode-map (kbd "<right>") 'hexl-my-forward-char)
(define-key hexl-mode-map (kbd "\C-f") 'hexl-my-backward-char)
(define-key hexl-mode-map (kbd "\C-b") 'hexl-my-forward-char)
