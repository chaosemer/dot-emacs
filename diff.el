;;; init/diff.el --- Diff mode customizations  -*- lexical-binding: t; -*-

;;; Declarations:
(declare-function diff-hl-show-hunk "diff-hl-show-hunk")
(declare-function ediff-setup-windows-plain "ediff-wind")
(defvar diff-hl-mode-map)
(defvar ediff-split-window-function)
(defvar ediff-window-setup-function)

;;; Code:
(setf ediff-split-window-function #'split-window-sensibly
      ediff-window-setup-function #'ediff-setup-windows-plain)

;; Restore window configuration when existing ediff.  Thanks to
;; https://www2.lib.uchicago.edu/keith/emacs/#org1c1fe56.
(defvar my-ediff-prev-window-configuration nil
  "The window configuration before entering ediff.")

(defun my-ediff-save-window-configuration ()
  (let ((window-config (current-window-configuration)))
    ;; Deleting other windows might error, so do it before modifying
    ;; the saved configuration.
    (delete-other-windows)
    (setf my-ediff-prev-window-configuration window-config)))
(defun my-ediff-restore-window-configuration ()
  (set-window-configuration my-ediff-prev-window-configuration))

(add-hook 'ediff-before-directory-setup-hooks
          #'my-ediff-save-window-configuration)
(add-hook 'ediff-before-setup-hook #'my-ediff-save-window-configuration)
(add-hook 'ediff-quit-hook #'my-ediff-restore-window-configuration)
(add-hook 'ediff-quit-session-group-hook
          #'my-ediff-restore-window-configuration)
(add-hook 'ediff-suspend-hook #'my-ediff-restore-window-configuration)

;; Don't show the registry when quitting directories
(defvar in-ediff-quit-meta-buffer nil)
(defun dont-show-registry-buffer-when-quitting-directories (old-fn)
 "Advice function for `ediff-quit-meta-buffer'.

OLD-FN: Old function, provided by advice system."
 (let ((in-ediff-quit-meta-buffer t))
   (funcall old-fn)))
(defun dont-show-registry (old-fn &rest _args)
  "Advice function for `ediff-show-registry'."
  (unless in-ediff-quit-meta-buffer
    (funcall old-fn)))

(advice-add 'ediff-quit-meta-buffer :around
            'dont-show-registry-buffer-when-quitting-directories)
(advice-add 'ediff-show-registry :around 'dont-show-registry)

;;; Kemaps:
;; I prefer customized interfaces for viewing diffs.
;;
;; Ediff by default. diff-hl for within a file.
(keymap-global-set "<remap> <vc-diff>" 'vc-ediff)
(with-eval-after-load 'diff-hl
  (keymap-set diff-hl-mode-map "<remap> <vc-diff>" #'diff-hl-show-hunk))

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
