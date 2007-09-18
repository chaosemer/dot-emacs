;;;; Compilation mode customizations
(hook-mode compilation-mode-hook
  next-error-follow-minor-mode)
(hook-mode occur-mode-hook
  next-error-follow-minor-mode)

(defun road-runner (&rest args)
  "BEEP! BEEP!"
  (dotimes (i 2)
    (beep) (sleep-for 0.2)))
(add-hook 'compilation-finish-functions 'road-runner)

(defvar compile-tetris nil
  "Boolean describing if tetris is active for a specific compilation buffer.")
(make-variable-buffer-local 'compile-tetris)

(defun compile-tetris (&rest args)
  "Play the tetris game, while compiling.

See also \\[tetris]"
  (interactive)
  (when (eq mode 'compilation-mode)
    (save-excursion
      (setf compile-tetris t))
    (tetris)))
(defun compile-tetris-end-game (buffer ignore)
  "Terminate the current game, no matter what buffer is active."
  (interactive)
  (when (buffer-local-value 'compile-tetris buffer)
    (let* ((tetris-buffer (get-buffer "*Tetris*"))
           (tetris-score (buffer-local-value 'tetris-score tetris-buffer))
           (tetris-score-file "compile-tetris-score"))
      (kill-buffer tetris-buffer)
      (switch-to-buffer buffer)
      (tetris-end-game)
      (setf compile-tetris nil))))
(setf compilation-process-setup-function 'compile-tetris)
(add-hook 'compilation-finish-functions 'compile-tetris-end-game t)