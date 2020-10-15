;;;; IDO customizations.  -*- lexical-binding: t; -*-
(ido-everywhere 1)
(ido-mode 1)

;;; ETags/ido integration
(defun ido-read-file-in-tags (prompt &optional require-match initial-input)
  "Return the name of a file in the current tags table.

Parameters have same meaning as in `ido-completing-read'."

  (let ((enable-recursive-minibuffers t))
    (visit-tags-table-buffer)
    (ido-completing-read prompt (tags-table-files) nil require-match initial-input)))

(defun ido-read-tag (prompt &optional require-match initial-input)
  "Return the name of a tag in the current tags table."

  (let ((enable-recursive-minibuffers t))
    (visit-tags-table-buffer)
    (ido-completing-read prompt
                         (let ((accum (list)))
                           (mapatoms (lambda (arg) (push (symbol-name arg) accum))
                                     (tags-completion-table))
                           accum)
                         nil require-match initial-input)))

(defun ido-find-tag-interactive ()
  (let ((default (symbol-at-point)))
    (list (ido-read-tag (format "Find tag: " default) nil
                        (and default (symbol-name default))))))

(defun ido-find-file-in-tags (file)
  (interactive (list (ido-read-file-in-tags "Find file: " t)))
  (find-tag file))
(defun ido-find-file-in-tags-other-window (file)
  (interactive (list (ido-read-file-in-tags "Find file: " t)))
  (find-tag-other-window file))
(defun ido-find-file-in-tags-other-frame (file)
  (interactive (list (ido-read-file-in-tags "Find file: " t)))
  (find-tag-other-frame file))

(defun ido-find-tag (tag)
  (interactive (ido-find-tag-interactive))
  (find-tag tag))
(defun ido-find-tag-other-window (tag)
  (interactive (ido-find-tag-interactive))
  (find-tag-other-window tag))
(defun ido-find-tag-other-frame (tag)
  (interactive (ido-find-tag-interactive))
  (find-tag-other-frame tag))

(defun ido-find-file-dwim (in-tags-p)
  "Interactively call `ido-find-file-in-tags' or `ido-find-file',
depending on the prefix arg"
  (interactive (list current-prefix-arg))
  (call-interactively
   (if in-tags-p 'ido-find-file-in-tags 'ido-find-file)))
(defun ido-find-file-other-window-dwim (in-tags-p)
  "Interactively call `ido-find-file-in-tags-other-window' or `ido-find-file-other-window',
depending on the prefix arg"
  (interactive (list current-prefix-arg))
  (call-interactively
   (if in-tags-p 'ido-find-file-in-tags-other-window 'ido-find-file-other-window)))
(defun ido-find-file-other-frame-dwim (in-tags-p)
  "Interactively call `ido-find-file-in-tags-other-frame' or `ido-find-file-other-frame',
depending on the prefix arg"
  (interactive (list current-prefix-arg))
  (call-interactively
   (if in-tags-p 'ido-find-file-in-tags-other-frame 'ido-find-file-other-frame)))

(let ((map (cdr ido-minor-mode-map-entry)))
  (define-key map (kbd "<remap> <find-tag>") 'ido-find-tag)
  (define-key map (kbd "<remap> <find-tag-other-window>") 'ido-find-tag-other-window)
  (define-key map (kbd "<remap> <find-tag-other-frame>") 'ido-find-tag-other-frame)
      
  (define-key map (kbd "<remap> <find-file>") 'ido-find-file-dwim)
  (define-key map (kbd "<remap> <find-file-other-window>") 'ido-find-file-other-window-dwim)
  (define-key map (kbd "<remap> <find-file-other-frame>") 'ido-find-file-other-frame-dwim))
