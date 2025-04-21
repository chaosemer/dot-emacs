;;; init/hexl.el --- Hexl (hex editor) customizations  -*- lexical-binding: t; -*-

;;; Declarations:
(declare-function hexl-activate-ruler "hexl")
(declare-function hexl-address-to-marker "hexl")
(declare-function hexl-ascii-start-column "hexl")
(declare-function hexl-backward-char "hexl")
(declare-function hexl-current-address "hexl")
(declare-function hexl-follow-line "hexl")
(declare-function hexl-forward-char "hexl")
(declare-function hexl-goto-address "hexl")
(declare-function hexl-insert-hex-string "hexl")
(declare-function hexl-insert-multibyte-char "hexl")
(declare-function hexl-line-displen "hexl")
(defvar hexl-max-address)
(defvar hexl-mode-map)

;;; Code:
(with-eval-after-load 'hexl
  (add-hook 'hexl-mode-hook
            (defun my-hexl-mode-hook ()
              (hexl-follow-line)
              (hexl-activate-ruler)
              (turn-on-eldoc-mode)
              (setf truncate-lines t))))

;;; Custom commands:
(with-eval-after-load 'hexl
  (keymap-set hexl-mode-map "<remap> <self-insert-command>"
    'hexl-my-self-insert-command)
  (keymap-set hexl-mode-map "C-m" 'hexl-my-self-insert-command)
  (keymap-set hexl-mode-map "<remap> <hexl-backward-char>"
    'hexl-my-backward-char)
  (keymap-set hexl-mode-map "<remap> <hexl-forward-char>"
    'hexl-my-forward-char)
  (keymap-set hexl-mode-map "<remap> <hexl-previous-line>"
    'hexl-my-previous-line)
  (keymap-set hexl-mode-map "<remap> <hexl-next-line>"
    'hexl-my-next-line)
  (keymap-set hexl-mode-map "<remap> <hexl-beginning-of-buffer>"
    'hexl-my-beginning-of-buffer)
  (keymap-set hexl-mode-map "<remap> <hexl-end-of-buffer>"
    'hexl-my-end-of-buffer)
  (keymap-set hexl-mode-map "<remap> <hexl-beginning-of-line>"
    'hexl-my-beginning-of-line)
  (keymap-set hexl-mode-map "<remap> <hexl-end-of-line>"
    'hexl-my-end-of-line))

;; TODO(upstream)
(defun hexl-insert-nybble (ch arg)
  "Insert nybble for character CH, ARG times.

CH: Character for a nybble to insert.  This should be a
    hexadecimal digit, ?0 - ?9 or ?A -?F.
ARG: Number of times to insert the above character."
  (when (not (or (<= ?0 ch ?9)
                 (<= ?a ch ?f)
                 (<= ?A ch ?F)))
    (error "Invalid nybble %c" ch))

  (dotimes (_ arg)
    (let* ((hex-position (hexl-address-to-marker (hexl-current-address)))
           (byte-str (buffer-substring hex-position (+ hex-position 2)))

           ;; nybble index
           (index (if (= (point) (1+ hex-position))
                       1 0)))
      (setf (aref byte-str index) ch)

      (save-excursion (hexl-insert-hex-string byte-str 1))
      (goto-char (+ hex-position index 1)))))

(defun hexl-my-self-insert-command (arg)
  "Replacement for `hexl-self-insert-command'.

ARG: Number of times to insert the character."
  (interactive "p")
  (if (< (current-column) (hexl-ascii-start-column))
      (hexl-insert-nybble last-command-event arg)
    (hexl-insert-multibyte-char last-command-event arg)))

(defun hexl-my-forward-char-internal (arg)
  "Move to right ARG bytes (left if ARG negative) in Hexl mode."
  (interactive "p")
  (let* ((hex-position (hexl-address-to-marker (hexl-current-address)))
         (index (if (= (point) (1+ hex-position))
                    1 0)))
    (hexl-goto-address (+ (hexl-current-address)
                          arg
                          (if (and (< arg 0) (/= index 0))
                              1 0)))))

(defun hexl-my-ascii-position (addr)
  "Return the buffer position in the ascii column.

ADDR: Address to get the position for, as returned from
`hexl-current-address'."
  (+ (* (/ addr 16) (hexl-line-displen))
     (hexl-ascii-start-column)
     (point-min)
     (% addr 16)))

(defun hexl-my-movement (f arg)
  "Execute movement function F, staying in ASCII or bytes column.

F: Function to execute.
ARG: Argument to pass to F, as (F ARG)."
  (let ((at-ascii-position (>= (current-column) (hexl-ascii-start-column))))
    (funcall f arg)
    (when at-ascii-position
      (goto-char (hexl-my-ascii-position (hexl-current-address t))))))

(defun hexl-my-movement0 (f)
  "Execute movement function F, staying in ASCII or bytes column.

F: Function to execute."
  (let ((at-ascii-position (>= (current-column) (hexl-ascii-start-column))))
    (funcall f)
    (when at-ascii-position
      (goto-char (hexl-my-ascii-position (hexl-current-address t))))))

(defun hexl-my-backward-char (arg)
  "Replacement for `hexl-backward-char'.

ARG: Number of characters to move."
  (interactive "p")
  (hexl-my-forward-char (- arg)))

(defun hexl-my-forward-char (arg)
  "Replacement for `hexl-forward-char'.

ARG: Number of characters to move."
  (interactive "p")
  (hexl-my-movement 'hexl-my-forward-char-internal arg))

(defun hexl-my-previous-line (arg)
  "Replacement for `hexl-previous-line'.

ARG: Number of lines to move."
  (interactive "p")
  (hexl-my-movement 'hexl-previous-line arg))

(defun hexl-my-next-line (arg)
  "Replacement for `hexl-next-line'.

ARG: Number of lines to move."
  (interactive "p")
  (hexl-my-movement 'hexl-next-line arg))

(defun hexl-my-beginning-of-buffer (arg)
  "Replacement for `hexl-beginning-of-buffer'.

ARG: Passed to `hexl-beginning-of-buffer'."
  (interactive "p")
  (hexl-my-movement 'hexl-beginning-of-buffer arg))

(defun hexl-my-end-of-buffer (arg)
  "Replcaement for `hexl-end-of-buffer'.

ARG: Passed to `hexl-end-of-buffer'."
  (interactive "p")
  (hexl-my-movement 'hexl-end-of-buffer arg))

(defun hexl-my-beginning-of-line ()
  "Replacement for `hexl-beginning-of-line'."
  (interactive)
  (hexl-my-movement0 'hexl-beginning-of-line))

(defun hexl-my-end-of-line ()
  "Replacement for `hexl-end-of-line'."
  (interactive)
  (hexl-my-movement0 'hexl-end-of-line))

(defun hexl-my-max-data-address ()
  "Return the maximum data address available.

Unlike `hexl-max-address', this is not rounded up to a whole
number of lines."
  (save-excursion
    (hexl-goto-address hexl-max-address)

    (if (= (point-min) (point-max))
        0
      ;; Actual calculation.
      (beginning-of-line)
      (forward-char 10)

      ;; Look for the end of the binary buffer.
      (while (and (< (hexl-current-address) hexl-max-address)
                  (/= (char-after) ?\s))
        (hexl-forward-char 1))
      (when (= (char-after) ?\s)
        (hexl-backward-char 1))
      (hexl-current-address))))
