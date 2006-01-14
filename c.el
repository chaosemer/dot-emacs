;; C ---------------------------------------------------------------------------
(hook-minor-mode c-mode-common-hook
  hide-ifdef-mode
  (c-set-offset 'case-label '+)
  (c-set-offset 'innamespace 0)
  (c-set-offset 'arglist-intro '++)
  (c-set-offset 'member-init-intro '++)
  (setf (local-key-binding (kbd "C-c M-<right>")) 'c-forward-conditional
        (local-key-binding (kbd "C-c M-<left>")) 'c-backward-conditional
        (local-key-binding (kbd "C-c M-<up>")) 'c-up-conditional-with-else
        (local-key-binding (kbd "C-c M-<down>")) 'c-down-conditional))

(setf (default-value 'c-recognize-knr-p) nil
      (default-value 'c-recognize-paren-inits) t
      (default-value 'c-recognize-<>-arglists) t)

(defvar pair-file-extension-alist
  (copy-tree '(("c" "h")
               ("h" "c" "cpp")
               ("cpp" "hpp" "h")
               ("hpp" "cpp")))
  "*Alist of extensions mapped to potential pair extensions.")

(defun pair-file-list (filename)
  (let ((extensions (cdr (assoc* (file-name-extension filename) file-extension-pair-alist
                                 :test #'string=))))
    (loop for extension in extensions
          collect (format "%s.%s"
                          (file-name-sans-extension filename)
                          extension))))

(defun* find-pair-file-1 (filename switch-fn)
  (let ((files (pair-file-list filename)))
    (when files
      (dolist (file files)
        (when (file-exists-p file)
          (return-from find-pair-file-1
            (funcall switch-fn (find-file-noselect file))))))
    (error "No known pair for file %s" filename)))

(defun find-pair-file-read-args (prompt)
  (list (read-file-name prompt nil buffer-file-name)))

(defun find-pair-file (filename)
  (interactive (find-pair-file-read-args "Find pair file of: "))
  (find-pair-file-1 filename #'switch-to-buffer))
(defun find-pair-file-other-window (filename)
  (interactive (find-pair-file-read-args "Find pair file in other window of: "))
  (find-pair-file-1 filename #'switch-to-buffer-other-window))
(defun find-pair-file-other-frame (filename)
  (interactive (find-pair-file-read-args "Find pair file in other frame of: "))
  (find-pair-file-1 filename #'switch-to-buffer-other-frame))

(defun switch-to-pair-file ()
  (interactive)
  (find-pair-file buffer-file-name))
(defun switch-to-pair-file-other-window ()
  (interactive)
  (find-pair-file-other-window buffer-file-name))
(defun switch-to-pair-file-other-frame ()
  (interactive)
  (find-pair-file-other-frame buffer-file-name))

(setf (global-key-binding (kbd "C-x C-h")) #'switch-to-pair-file
      (global-key-binding (kbd "C-x 4 C-h")) #'switch-to-pair-file-other-window
      (global-key-binding (kbd "C-x 4 h")) #'switch-to-pair-file-other-window
      (global-key-binding (kbd "C-x 5 C-h")) #'switch-to-pair-file-other-frame
      (global-key-binding (kbd "C-x 5 h")) #'switch-to-pair-file-other-frame)