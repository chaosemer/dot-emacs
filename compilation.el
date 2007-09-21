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

;;; Compile/Tetris commands.  Play Tetris while waiting for that build
;;; to finish.
(defun compile-tetris-window-list ()
  "Returns a list of two windows, for compilation and Tetris, respectively.

If there are not enough windows on the current frame, create
enough windows."
  (let (current compilation tetris)
    (setf current (selected-window))
    (let ((next (next-window current)))
      (setf compilation (if (not (eq next current)) next
                          (split-window current))))
    (let ((next (next-window compilation)))
      (setf tetris (if (not (member next (list current compilation)))
                       next
                     (split-window compilation nil t))))

    (list compilation tetris)))

(defun compile-tetris (command &optional comint)
  "Start compiling and Tetris.

Compliing is started as in `compile', and Tetris is started as in
`tetris'.  Paramters have same meaning as `compile'."
  (interactive
   (list
    (let ((command (eval compile-command)))
      (if (or compilation-read-command current-prefix-arg)
          (read-from-minibuffer "Compile command: "
                                command nil nil
                                (if (equal (car compile-history) command)
                                    '(compile-history . 1)
                                  'compile-history))
        command))
    (consp current-prefix-arg)))

  (add-hook 'compilation-finish-functions 'compile-tetris-compilation-finished)

  (destructuring-bind (compilation tetris)
      (compile-tetris-window-list)
    (set-window-buffer compilation (get-buffer-create "*compilation*"))
    (set-window-buffer tetris (get-buffer-create "*Compile-Tetris*"))

    ;; start up compilation
    (let ((compilation-buffer-name-function (lambda (mode) "*compilation*")))
      (compile command comint))

    ;; start up Tetris
    (select-window tetris)
    (gamegrid-kill-timer)
    (tetris-mode)
    (tetris-start-game)))

(defun recompile-tetris ()
  "`compile-tetris' for `recompile'."
  (interactive)

  (let ((mode (second compilation-arguments)))
    (cond ((eq mode 'compilation-mode)
           (compile-tetris (first compilation-arguments)
                           (second compilation-arguments)))
          ((eq mode nil)
           (compile-tetris compile-command))
          (t
           (recompile)))))

(defun compile-tetris-compilation-finished (buffer str)
  "Function called when a `compile-tetris' compilation is finished."
  (save-excursion
    (set-buffer (get-buffer-create "*Compile-Tetris*"))
    (compile-tetris-end-game)))

(defun compile-tetris-end-game ()
  (gamegrid-kill-timer)
  (use-local-map (make-sparse-keymap))
  ;; should something be done with the highscore?
  )

;; override the compile/recompile commands to use compile-tetris and
;; recompile-tetris
(setf (global-key-binding (kbd "<f7>")) 'recompile-tetris
      (global-key-binding (kbd "C-<f7>")) 'compile-tetris)