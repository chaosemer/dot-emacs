;;;; Patches for buggy/old emacs code
;;;;
;;;; This allows me to use one init file across multiple versions of Emacs.
;;;;
;;;; Currently tested against:
;;;; * GNU Emacs 22.0.50.1, 2006-01-14

(unless (fboundp 'warn)
  (message "Using old compatibility mode for `warn'")
  (defalias 'warn 'message))

(unless (fboundp 'cua-mode)
  (require 'cua)
  (warn "Using old compatibility mode for `cua-mode'")
  (defalias 'cua-mode 'CUA-mode))

(unless (fboundp 'grep-tree)
  (warn "Using old compatibility mode for `grep-tree'")
  (defalias 'grep-tree 'grep-find))

(unless (fboundp 'global-semantic-idle-completions-mode)
  (warn "Using old compatibility mode for `global-semantic-idle-completions-mode'")
  (defalias 'global-semantic-idle-completions-mode 'global-semantic-idle-scheduler-mode))

(unless (fboundp 'semantic-default-elisp-setup)
  (warn "Using non-existant function `semantic-default-elisp-setup'")
  (defun semantic-default-elisp-setup ()))

(unless (fboundp 'x-show-tip)
  (warn "Modifying `tooltip-mode' to do nothing or error.")
  (defun tooltip-mode (&optional arg)
    (interactive (list (prefix-numeric-value current-prefix-arg)))
    (when (> arg 0)
      (error "Sorry, tooltips are not yet available on this system"))))

(unless (fboundp 'custom-autoload)
  (warn "Using old compatibility mode for `custom-autoload'")
  (defalias 'custom-autoload 'custom-add-load))

(let ((move-fns '(backward-sexp forward-sexp backward-up-list up-list down-list
                  c-forward-conditional c-backward-conditional
                  c-down-conditional c-up-conditional
                  c-down-conditional-with-else
                  c-up-conditional-with-else)))
  (unless (every (lambda (symbol) (eq 'move (get symbol 'CUA)))
                 move-fns)
    (warn "Adding CUA property to all symbols that need it.")
    (dolist (symbol move-fns)
      (setf (get symbol 'CUA) 'move))))

(unless (fboundp 'semantic-complete-jump-other-window)
  (warn "Creating missing function `semantic-complete-jump-other-window")
  (defun semantic-complete-jump-other-window ()
    "Jump to a semantic symbol."
    (interactive)
    (let* ((semanticdb-search-system-databases nil)
           (tag (semantic-complete-read-tag-project "Symbol: ")))
      (when (semantic-tag-p tag)
        (push-mark)
        (semantic-go-to-tag tag)
        (switch-to-buffer-other-window (current-buffer))
        (semantic-momentary-highlight-tag tag)
        (working-message "%S: %s "
                         (semantic-tag-class tag)
                         (semantic-tag-name  tag))))))

(unless (fboundp 'semantic-complete-jump-other-frame)
  (defun semantic-complete-jump-other-frame ()
    "Jump to a semantic symbol."
    (interactive)
    (let* ((semanticdb-search-system-databases nil)
           (tag (semantic-complete-read-tag-project "Symbol: ")))
      (when (semantic-tag-p tag)
        (push-mark)
        (semantic-go-to-tag tag)
        (switch-to-buffer-other-frame (current-buffer))
        (semantic-momentary-highlight-tag tag)
        (working-message "%S: %s "
                         (semantic-tag-class tag)
                         (semantic-tag-name  tag))))))

(unless (fboundp 'global-hi-lock-mode)
  (warn "Using old compatibility mode for GLOBAL-HI-LOCK-MODE")
  (defalias 'global-hi-lock-mode 'hi-lock-mode))

(progn
  (warn "Updating `semantic-show-parser-state-marker' to be more GUItiful.")
  ;; Update semantic-show-parser-state-marker
  (require 'semantic-util-modes)
  (setf (get 'semantic-show-parser-state-string 'risky-local-variable) t)
  (defun semantic-show-parser-state-marker (&rest ignore)
    "Set `semantic-show-parser-state-string' to indicate parser state.
This marker is one of the following:
 `-'  ->  The cache is up to date.
 `!'  ->  The cache requires a full update.
 `~'  ->  The cache needs to be incrementally parsed.
 `%'  ->  The cache is not currently parseable.
 `@'  ->  Auto-parse in progress (not set here.)
Arguments IGNORE are ignored, and accepted so this can be used as a hook
in many situations."
    (labels ((make-state-string (string &optional (help-echo "") func)
                                (if func
                                    (let ((map (make-sparse-keymap)))
                                      (setf (lookup-key map (kbd "<mode-line> <mouse-1>")) func)
                                      (propertize string
                                                  'help-echo help-echo
                                                  'mouse-face 'mode-line-highlight
                                                  'local-map map))
                                  (propertize string 'help-echo help-echo))))
      (setf semantic-show-parser-state-string
            (cond ((semantic-parse-tree-needs-rebuild-p) 
                   (make-state-string "!" "Needs a full parse: mouse-1 reparses"
                                      (lambda () (interactive) (semantic-refresh-tags-safe) nil)))
                  ((semantic-parse-tree-needs-update-p)
                   (make-state-string "^" "Needs an incremental parse: mouse-1 reparses"
                                      (lambda () (interactive) (semantic-refresh-tags-safe) nil)))
                  ((semantic-parse-tree-unparseable-p)
                   (make-state-string "%" "Buffer Unparsable: mouse-1 reparses"
                                      (lambda () (interactive) (semantic-refresh-tags-safe) nil)))
                  (t
                   (make-state-string "-" "Semantic is up to date")))
            ))
    ;;(message "Setup mode line indicator to [%s]" semantic-show-parser-state-string)
    (semantic-mode-line-update)))

(progn
  (warn "Making `semantic-complete-read-tag-engine' have sensible default behavior")
  (require 'semantic-complete)
  (defun semantic-complete-read-tag-engine (collector displayor prompt
                                                      default-tag initial-input
                                                      history)
    "Read a semantic tag, and return a tag for the selection.
Argument COLLECTOR is an object which can be used to to calculate
a list of possible hits.  See `semantic-completion-collector-engine'
for details on COLLECTOR.
Argumeng DISPLAYOR is an object used to display a list of possible
completions for a given prefix.  See`semantic-completion-display-engine'
for details on DISPLAYOR.
PROMPT is a string to prompt with.
DEFAULT-TAG is a semantic tag or string to use as the default value.
If INITIAL-INPUT is non-nil, insert it in the minibuffer initially.
HISTORY is a symbol representing a variable to story the history in."
    (let* ((semantic-completion-collector-engine collector)
           (semantic-completion-display-engine displayor)
           (semantic-complete-active-default nil)
           (semantic-complete-current-matched-tag nil)
           (ans nil)
           (tag nil)
           (default-as-tag (semantic-complete-default-to-tag default-tag))
           (default-as-string (when (semantic-tag-p default-as-tag)
                                (semantic-tag-name default-as-tag)))
           )

      (when (and (null initial-input) default-as-string)
        (psetf initial-input default-as-string
               default-as-string nil))
      (when default-as-string
        ;; Add this to the prompt.
        ;;
        ;; I really want to add a lookup of the symbol in those
        ;; tags available to the collector and only add it if it
        ;; is available as a possibility, but I'm too lazy right
        ;; now.
        ;;
        (if (string-match ":" prompt)
            (setq prompt (concat
                          (substring prompt 0 (match-beginning 0))
                          " (" default-as-string ")"
                          (substring prompt (match-beginning 0))))
          (setq prompt (concat prompt " (" default-as-string "): "))))
      ;;
      ;; Perform the Completion
      ;;
      (setq ans
            (read-from-minibuffer prompt
                                  initial-input
                                  semantic-complete-key-map
                                  nil
                                  (or history
                                      'semantic-completion-default-history)
                                  default-tag))
      ;;
      ;; Extract the tag from the completion machinery.
      ;;
      semantic-complete-current-matched-tag
      )))

(progn
  (warn "Adding setf expansion for `define-key'")
  (defsetf lookup-key define-key))
