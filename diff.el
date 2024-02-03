;;; init/diff.el --- Diff mode customizations  -*- lexical-binding: t; -*-

;;; Code:
;; no special code

;;; Kemaps:
;; VC should use EDiff, as it provides a better visualization.
(keymap-global-set "<remap> <vc-diff>" 'vc-ediff)

;;; Custom commands:
(defun hexl-find-file-noselect (file)
  "Find a file and put it into `hexl-mode'.

FILE: Path to a file to find."
    (let ((buffer (find-file-noselect file nil t)))
      (with-current-buffer buffer
        (hexl-mode 1))
      buffer))

(defun ediff-binary-files (file1 file2)
  "Diff two binary files with `ediff'.

The diff will be done on the binary contents with both buffers in
`hexl-mode'.

FILE1: Path to the first file.
FILE2: Path to the second file."
  (interactive (list (read-file-name "File A to compare: ")
                     (read-file-name "File B to compare: ")))
  (apply #'ediff-buffers (mapcar #'hexl-find-file-noselect (list file1 file2))))

(defun ediff-binary-files3 (file1 file2 file3)
  "Diff three binary files with `ediff3'.

The diff will be done on the binary contents with all buffers in
`hexl-mode'.

FILE1: Path to the first file.
FILE2: Path to the second file.
FILE3: Path to the third file."
  (interactive (list (read-file-name "File A to compare: ")
                     (read-file-name "File B to compare: ")
                     (read-file-name "File C to compare: ")))
  (apply #'ediff-buffers3 (mapcar #'hexl-find-file-noselect (list file1 file2 file3))))
