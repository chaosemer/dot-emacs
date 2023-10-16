;;;; Text mode customizations.  -*- lexical-binding: t; -*-
;;;;
;;;; Why not use Emacs for editting plain old text?
(hook-mode text-mode-hook
  (unless (member major-mode '(mail-mode org-mode))
	visual-line-mode))

(hook-mode markdown-mode-hook
  ;; Normally, electric pair mode is on and helpful, but in Markdown
  ;; mode it makes URLs be automatically hidden before they're typed
  ;; in.
  (electric-pair-local-mode -1)

  (setf markdown-hide-markup t
        ;; So URLs hide automatically as they are typed.
        markdown-hide-urls t))

;; Make sure we delete selection on highlight.
(put 'markdown-enter-key 'delete-selection t)

;; Make markdown mode always indent / unintent with TAB / S-TAB
(defun my-markdown-indent-line ()
  "Wrapper for `markdown-indent-line' that always does indentation."
  (interactive)

  ;; markdown-indent-line checks this-command, so we have to override it.
  (let ((this-command 'markdown-cycle))
    (markdown-indent-line)))

(defun my-markdown-outdent-line ()
  "Replacement that always outdents the current line."
  (interactive)
  (save-excursion
    (back-to-indentation)
    (indent-line-to (markdown-outdent-find-next-position
                     (current-column)
                     (markdown-calc-indents)))))

(with-eval-after-load 'markdown-mode
  (keymap-set markdown-mode-map "TAB" 'my-markdown-indent-line)
  (keymap-set markdown-mode-map "<backtab>" 'my-markdown-outdent-line))
