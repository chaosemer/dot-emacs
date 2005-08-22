(defun hexl-find-file-noselect (file)
  "Find a file and put it into hexl-mode."
    (let ((buffer (find-file-noselect file nil t)))
      (with-current-buffer buffer
        (hexl-mode 1))
      buffer))

(defun ediff-binary-files (file1 file2)
  (interactive (list (read-file-name "File A to compare: ")
                     (read-file-name "File B to compare: ")))
  (apply #'ediff-buffers (mapcar #'hexl-find-file-noselect (list file1 file2))))

(defun ediff-binary-files3 (file1 file2 file3)
  (interactive (list (read-file-name "File A to compare: ")
                     (read-file-name "File B to compare: ")
                     (read-file-name "File C to compare: ")))
  (apply #'ediff-buffers3 (mapcar #'hexl-find-file-noselect (list file1 file2 file3))))