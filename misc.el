(require 'cl)

(defun scratch ()
  "Switch to the scratch buffer."
  (interactive)
  (display-buffer (get-buffer-create "*scratch*") nil t))

(defconst read-only-color "pink")
(defconst normal-color (face-background 'default))
(defconst read-only-fringe-color "pink2")
(defconst normal-fringe-color (face-background 'fringe))

(defun update-background-color ()
  ;; don't update if not in a file buffer
  (when (or buffer-file-name (string-equal (buffer-name) "*scratch*"))
    (multiple-value-bind (new-bg-color new-fringe-color)
        (if buffer-read-only
            (values read-only-color read-only-fringe-color)
          (values normal-color normal-fringe-color))
      
      (when (not (string= new-bg-color (face-background 'default)))
        (setf (face-background 'default) new-bg-color))
      (when (not (string= new-fringe-color (face-background 'fringe)))
        (setf (face-background 'fringe) new-fringe-color)))))

;(when window-system
;  (run-at-time t 0.3 'update-background-color))

;(defadvice display-buffer (around around-display-buffer)
;  (let ((pop-up-frames (buffer-file-name (get-buffer (ad-get-arg 0)))))
;    ad-do-it))
;
;(defadvice switch-to-buffer (around around-switch-to-buffer)
;  (display-buffer FIXME))
