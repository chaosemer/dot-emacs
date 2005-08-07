(require 'ediff)

(defmacro+ with-buffer (buffer &body body)
  "Temporarily make BUFFER the active buffer"
  `(save-excursion
     (set-buffer ,buffer)
     ,@body))

(defun hexl-find-file-noselect (file)
  "Find a file and put it into hexl-mode."
    (let ((buffer (find-file-noselect file nil t)))
      (with-buffer buffer
        (hexl-mode 1))
      buffer))

(defun ediff-binary-files (file1 file2)
  (interactive (list (read-file-name "File A to compare: ")
                     (read-file-name "File B to compare: ")))
  
  (let ((buffer1 (hexl-find-file-noselect file1))
        (buffer2 (hexl-find-file-noselect file2)))
    (ediff-buffers buffer1 buffer2)))