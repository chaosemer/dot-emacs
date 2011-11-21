;;;; Customizing Version Control menus and such
(require 'vc-git)
(define-key vc-git-extra-menu-map [git-svn-dcommit]
  '(menu-item "Git SVN Commit" vc-git-svn-dcommit))
(define-key vc-git-extra-menu-map [git-svn-rebase]
  '(menu-item "Git SVN Update" vc-git-svn-rebase))

(defun vc-git-svn-rebase ()
  "Get lastest changes from SVN to Git."
  (interactive)
  (let ((root (vc-git-root default-directory)))
	(when root
	  (vc-git--call nil "svn" "rebase"))))
(defun vc-git-svn-dcommit (&optional rebase-also)
  "Commit changes stored in a Git repository to SVN."
  (interactive "P")
  (let ((root (vc-git-root default-directory)))
	(when root
	  (if rebase-also
		  (vc-git--call nil "svn" "dcommit")
		(vc-git--call nil "svn" "dcommit" "--no-rebase")))))