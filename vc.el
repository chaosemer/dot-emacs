;;; init/vc.el --- Customizing Version Control menus and such  -*- lexical-binding: t; -*-

;;; Code:

;;; Custom commands:
(with-eval-after-load 'vc-git
  (keymap-set vc-git-extra-menu-map "<git-svn-dcommit>"
              '(menu-item "Git SVN Commit" vc-git-svn-dcommit))
  (keymap-set vc-git-extra-menu-map "<git-svn-rebase>"
              '(menu-item "Git SVN Update" vc-git-svn-rebase)))

(defun vc-git-svn-rebase ()
  "Get latest commits from SVN to Git."
  (interactive)
  (let ((root (vc-git-root default-directory)))
    (assert root nil "Not in a Git repository")
    (vc-git-command "*vc-git*" 'async nil "svn" "rebase")
    (display-buffer "*vc-git*")))

(defun vc-git-svn-dcommit (&optional rebase-also)
  "Submit SVN commits for all commits in the Git repository.

REBASE-ALSO: If non-nil, do not actually rebase so that you can
manually do the desired rebase."
  (interactive "P")
  (let ((root (vc-git-root default-directory)))
    (assert root nil "Not in a Git repository")
    (if rebase-also
	(vc-git-command "*vc-git*" 'async nil "svn" "dcommit" "--no-rebase")
      (vc-git-command "*vc-git*" 'async nil "svn" "dcommit"))
    (display-buffer "*vc-git*")))
