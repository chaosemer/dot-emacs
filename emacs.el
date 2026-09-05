;;; init/emacs.el --- Global Emacs customizations  -*- lexical-binding: t; -*-

;;; Declarations:
(declare-function ibuffer-do-sort-by-alphabetic "ibuf-ext")
(declare-function ibuffer-switch-to-saved-filter-groups "ibuf-ext")
(declare-function ibuffer-toggle-filter-group "ibuf-ext")
(declare-function tool-bar-mode "tool-bar")
(defvar completion-eager-display)
(defvar completion-eager-update)
(defvar ibuffer-hidden-filter-groups)
(defvar ibuffer-mode-filter-group-map)
(defvar ibuffer-saved-filter-groups)
(defvar ibuffer-show-empty-filter-groups)
(defvar init-dir--long-load-time-warning)
(defvar markdown-header-scaling)
(defvar outline-minor-mode-use-buttons)
(defvar pixel-scroll-precision-interpolate-page)
(defvar tool-bar-map)
(defvar vc-find-revision-no-save)
(defvar view-lossage-auto-refresh)
(defvar x-stretch-cursor)
(defvar xterm-extra-capabilities)
(defvar xterm-set-window-title)

;;; Code:
(eval-and-compile
  (defun stub-function (sym package)
    "Define a stub replacement for SYM if needed."
    (unless (fboundp sym)
      (display-warning 'emacs (format "SETUP ISSUE: %s package is not installed." package))
      (fset sym (lambda (&optional _)
                  ;; Do nothing -- stub
                  ))))
  (stub-function 'diff-hl-flydiff-mode "diff-hl")
  (stub-function 'diff-hl-margin-mode "diff-hl")
  (stub-function 'global-form-feed-st-mode "form-feed-st")
  (stub-function 'global-kkp-mode "kkp")
  (stub-function 'global-window-tool-bar-mode "window-tool-bar")
  (stub-function 'global-diff-hl-mode "diff-hl")
  (stub-function 'global-diff-hl-show-hunk-mouse-mode "diff-hl"))

;; Ensure doc-view can be used
(unless (and (executable-find "dvipdf")
             (executable-find "pdftotext"))
  (display-warning 'emacs "SETUP ISSUE: dvipdf and pdftotext programs are not installed."))

;; This file is known to be slow, so add a bit more time here.
(cl-incf init-dir--long-load-time-warning 0.1)

;;; Package customization:

;; Intended archive sequencing:
;;
;; 1. melpa-stable
;; 2. Emacs default
;; 3. melpa (HEAD)
;;
;; Melpa (HEAD) should never auto update.
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archive-priorities
             '("melpa-stable" . 100))
(add-to-list 'package-archive-priorities
             '("melpa" . -100))
(setf package-archive-column-width 12)

;; Refreshing the list of packages takes even longer than calculating
;; the list (it involves network traffic) so run that asynchronously
;; when it won't impact user interaction.
(run-with-idle-timer
 30 nil
 (lambda () (package-refresh-contents t)))

;;; Global customizations:

(column-number-mode 1)
(context-menu-mode 1)
(cua-mode 1)
(editorconfig-mode 1)
(electric-pair-mode 1)
(if (string-version-lessp emacs-version "31.1")
    (fido-mode 1)
  (setf completion-eager-display t
        completion-eager-update t
        completion-ignore-case t
        read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completions-max-height 20
        completions-detailed t
        completion-auto-help 'always
        completion-show-help nil
        completions-format 'one-column
        minibuffer-visible-completions t)

  ;; Minibuffer complete on RET in all cases, but allow C-u RET to do
  ;; allow non-completion
  (defun my-minibuffer-complete-and-exit (dont-do-completion)
    (interactive "P")
    (if dont-do-completion
        (exit-minibuffer)
      (let ((minibuffer-completion-confirm nil))
        (minibuffer-complete-and-exit))))
  (keymap-set minibuffer-mode-map "<remap> <minibuffer-complete-and-exit>"
              'my-minibuffer-complete-and-exit))
(global-form-feed-st-mode 1)
(global-goto-address-mode 1)
(global-kkp-mode 1)
(global-subword-mode 1)
;; TODO(emacs31) Function only available on Emacs 31
(when (fboundp 'global-xref-mouse-mode)
  (global-xref-mouse-mode 1))
(setf (default-value 'indent-tabs-mode) nil
      tab-always-indent 'complete)
;; TODO(emacs31) Function only available on Emacs 31
(when (fboundp 'package-autosuggest-mode)
  (package-autosuggest-mode 1))
(recentf-mode 1)
(repeat-mode 1)
(save-place-mode 1)
(savehist-mode 1)
(tooltip-mode -1)
(url-handler-mode 1)

(defvar mode-line-collapse-minor-modes) ;TODO(Only needed pre-Emacs 31)
(setf mode-line-collapse-minor-modes
      '(eldoc-mode
        form-feed-st-mode
        subword-mode
        visual-line-mode))

;; Diff-hl is particularly heavyweight (2025-03-31)
;; Defer until the first file is loaded.
;;
;; TODO(upstream) -- All of these are global modes. Global modes
;; should be cheap to turn on.
(defvar my-turn-on-diff-hl-mode nil)
(add-hook 'find-file-hook (defun my-turn-on-diff-hl-mode ()
                            (unless my-turn-on-diff-hl-mode
                              (global-diff-hl-mode 1)
                              (global-diff-hl-show-hunk-mouse-mode 1)
                              (diff-hl-flydiff-mode 1)
                              (setf my-turn-on-diff-hl-mode t))))

;; I prefer splitting windows horizontally where possible
(setf split-width-threshold 150         ;twice 75, which is a bit under
                                        ;an 80 column wide value to
                                        ;make room for margins
      split-height-threshold nil)

;; This only works on Linux
(defvar font-use-system-font) ; Only exists in builds compiled with
                              ; display support.
(setf font-use-system-font t)

;; Bar cursor toggling (abandon bar-cursor-mode)
(defun update-cursor-type ()
  (setf cursor-type (if overwrite-mode 'box t)))
(add-hook 'overwrite-mode-hook #'update-cursor-type)

;; Toolbar display

;; tool-bar-mode is not preloaded on emacs-nox builds
(unless (fboundp 'tool-bar-mode)
  (require 'tool-bar))
(global-window-tool-bar-mode 1)
(tool-bar-mode -1)
(setf tool-bar-map nil)

(if window-system
    (progn (pixel-scroll-precision-mode 1)
           (setf pixel-scroll-precision-interpolate-page t)

           (when (string-match "microsoft" (shell-command-to-string "uname -r"))
             ;; Disable the primary selection, it breaks Emacs on
             ;; WSLg.
             ;; https://www.lukas-barth.net/blog/emacs-wsl-copy-clipboard/
             (setq select-active-regions nil)))
  (xterm-mouse-mode 1)
  (diff-hl-margin-mode 1)
  ;; TODO(upstream): Not yet available in Emacs -- see bug#80091
  (defvar xterm-update-cursor)

  (setf xterm-set-window-title t
        xterm-update-cursor t)
  (when (string-match "microsoft" (shell-command-to-string "uname -r"))
    ;; Windows Console does not properly report that it supports
    ;; setSelection or reportBackground.
    (setq xterm-extra-capabilities '(reportBackground setSelection))

    ;; Use the default Windows browser.
    (setq browse-url-browser-function (lambda (url &rest _)
                                        (call-process "explorer.exe" nil nil nil url)))))

;; Make Emacs display similar to modern editors.
(setf frame-resize-pixelwise t
      frame-title-format "%b - Emacs"
      frame-inhibit-implied-resize t
      icon-title-format t
      scroll-conservatively most-positive-fixnum
      window-resize-pixelwise t
      x-stretch-cursor t)
(add-hook 'prog-mode-hook
          (defun my-prog-mode-hook ()
            (setf show-trailing-whitespace t)))

(setf delete-by-moving-to-trash t
      dired-auto-revert-buffer t
;; BUG: This breaks M-x customize-themes visuals
;;       form-feed-st-include-modes '(prog-mode text-mode special-mode)
      mouse-drag-and-drop-region t
      mouse-drag-and-drop-region-cross-program t
      mouse-drag-mode-line-buffer t
      narrow-to-defun-include-comments t
      outline-minor-mode-use-buttons 'in-margins
      parse-sexp-lookup-properties t
      save-interprogram-paste-before-kill t
      sentence-end-double-space nil     ;Except ELisp, see elisp.el
      shell-command-prompt-show-cwd t
      truncate-partial-width-windows nil
      use-dialog-box nil
      use-short-answers t
      vc-find-revision-no-save t
      view-lossage-auto-refresh t)
(modify-all-frames-parameters '((cursor-type . bar)))

;; Ignore Unity .meta files as well, they show up everywhere.
(add-to-list 'completion-ignored-extensions ".meta")

;; Customize the *scratch* buffer
(add-hook 'emacs-startup-hook
          (defun my-emacs-startup-hook ()
            (with-current-buffer (get-buffer "*scratch*")
              (setf buffer-offer-save t))))
(setf initial-major-mode 'gfm-mode
      initial-scratch-message (concat "# Scratch buffer for notes\n"
                                      "\n"
                                      "Put notes here..."))

;; When deugging xterm-mouse issues, having a large buffer is quite
;; helpful.
(lossage-size 10000)

;;; Faces:

(setf (face-background 'show-paren-match)
      (if (> (display-color-cells) 256) "light gray" "blue")
      (face-background 'show-paren-mismatch) "red"
      (face-foreground 'show-paren-mismatch) "white"
      (face-background 'vertical-border) "#2d2d2d")

;;; Section and File level comment functionality.
;;;
;;; This allows for section and file level comments to be rendered bigger than normal text, so they
;;; stand out a bit more.
(defface section-comment-face
  '((t (:height 1.3 :inherit (font-lock-comment-face variable-pitch))))
  "Face for section level comments."
  :group 'local)
(defface file-comment-face
  '((t (:height 1.5 :weight bold :inherit (font-lock-comment-face variable-pitch))))
  "Face for file level comments."
  :group 'local)

;; Also make same adjustments for markdown.
(setf markdown-header-scaling t)

;;; Keymaps:

;; indent on newline
(keymap-global-set "C-j" 'newline)
(keymap-global-set "RET" 'newline-and-indent)

;; usual editor bindings
(keymap-global-set "C-f" 'occur)
(keymap-global-set "C-S-f" 'rgrep)
(keymap-global-set "C-g" 'goto-line)
(keymap-global-set "<f7>" 'recompile)
(keymap-global-set "C-<f7>" 'compile)
(keymap-global-set "S-<f7>" 'kill-compilation)
(keymap-global-set "C-a" 'mark-whole-buffer)
(keymap-global-set "M-<home>" 'beginning-of-defun)
(keymap-global-set "M-<end>" 'end-of-defun)

;; simpler sexp bindings
(keymap-global-set "M-<right>" 'forward-sexp)
(keymap-global-set "M-<left>" 'backward-sexp)
(keymap-global-set "M-<up>" 'backward-up-list)
(keymap-global-set "M-<down>" 'down-list)
(keymap-global-set "M-SPC" 'mark-sexp)
(keymap-global-set "M-<delete>" 'kill-sexp)
(keymap-global-set "M-<backspace>" 'backward-kill-sexp)

;; Fancier paste (cua-specific)
(keymap-global-set "<remap> <delete-selection-repeat-replace-region>" 'cua-paste-pop)

;; I'm always mistakenly hitting these
(dolist (key '("C-<next>" "C-<prior>" "C-x m" "M-<home>" "M-<end>" "M-<begin>" "C-x <left>"
               "C-x <right>" "M-<begin>" "M-<next>" "M-<prior>" "C-M-v" "C-M-S-v" "ESC <begin>"
               "ESC <end>" "ESC <home>" "ESC <next>" "ESC <prior>"))
  (keymap-global-unset key))
(setq-default mode-line-buffer-identification
              (mapcar (lambda (str) (substring-no-properties str))
                      mode-line-buffer-identification))

;; Prefer creating new windows horizontally
(keymap-global-set "C-x 2" 'split-window-right)
(keymap-global-set "C-x 3" 'split-window-below)

;; Use ibuffer for buffer management
(keymap-global-set "C-x C-b" 'ibuffer)

;; My experimental package, <http://github.com/chaosemer/window-tool-bar>
(require 'window-tool-bar)
(keymap-global-set "C-x C-m" #'window-tool-bar-debug-show-memory-use)

;;; Custom commands:

;;; DWIM <home> and <end> TODO(package)
(defun beginning-of-line-dwim (&optional _)
  "Move point to the first non-whitespace character or the beginning of line."
  (interactive "^p")

  (let ((point (point)))
    (back-to-indentation)
    (when (= point (point))
      (beginning-of-line))))

(defun end-of-line-dwim (&optional n)
  "Movie point to the last non-whitespace character or the end of line.

N: Number of lines to go forward."
  (interactive "^p")

  (let ((point (point)))
    (end-of-line n)
    (skip-chars-backward " \t")
    (when (= point (point))
      (end-of-line))))

(keymap-global-set "<home>" 'beginning-of-line-dwim)
(keymap-global-set "<end>" 'end-of-line-dwim)

;;; For whatever reason, my mind doesn't really align with registers
;;; to store window configuration.  Instead, I like to think about
;;; things as a stack.

;; New option: winner-mode
;;
;; The main benefit of this is it already exists in Emacs and is
;; widely used.
(winner-mode 1)

;; Old option: Recursive edits
;;
;; This is self written.  It's been pretty stable.
(defun push-or-pop-excursion (arg)
  "Pushes or pops an excursion, depending on the prefix arg.

ARG: If nil (the default), then push an excursion.  Otherwise,
pop an excursion."
  (interactive (list current-prefix-arg))

  (if (not arg)
      (save-excursion (save-restriction (save-window-excursion (recursive-edit))))
    (when (> (recursion-depth) 0)
      (throw 'exit 'nil))))
(keymap-global-set "C-x C-p" 'push-or-pop-excursion)

;;; Sibling file navigation.  This is helpful with C / C++ code when
;;; switching between header and code files.
(defun find-sibling-file-other-window (file)
  "Variant of `find-sibling-file', that opens in another window.

When called interactively, find the sibling of the current
buffer's file.

FILE: File to find the sibling file of."
  (interactive (progn
                 (unless buffer-file-name
                   (user-error "Not visiting a file"))
                 (list buffer-file-name)))
  (let ((display-buffer-overriding-action
         '(display-buffer-pop-up-window . ((inhibit-same-window . t)))))
    (find-sibling-file file)))

(defun find-sibling-file-other-frame (file)
  "Variant of `find-sibling-file', that opens in another frame.

When called interactively, find the sibling of the current
buffer's file.

FILE: File to find the sibling file of."
  (interactive (progn
                 (unless buffer-file-name
                   (user-error "Not visiting a file"))
                 (list buffer-file-name)))
  (let ((display-buffer-overriding-action
         '(display-buffer-pop-up-frame . ((inhibit-same-window . t)))))
    (find-sibling-file file)))

(setf find-sibling-rules
      '(("\\([^/]+\\)\\.c$" "\\1.h")
        ("\\([^/]+\\)\\.cc$" "\\1.h" "\\1.hh")
        ("\\([^/]+\\)\\.cpp$" "\\1.h" "\\1.hpp")
        ("\\([^/]+\\)\\.h$" "\\1.c" "\\1.cpp" "\\1.cc")
        ("\\([^/]+\\)\\.hh$" "\\1.cc")
        ("\\([^/]+\\)\\.hpp$" "\\1.cpp")))

(keymap-global-set "C-x C-h" 'find-sibling-file)
(keymap-global-set "C-x 4 C-h" 'find-sibling-file-other-window)
(keymap-global-set "C-x 4 h" 'find-sibling-file-other-window)
(keymap-global-set "C-x 5 C-h" 'find-sibling-file-other-frame)
(keymap-global-set "C-x 5 h" 'find-sibling-file-other-frame)

;;; Turn on the horizontal scroll bar when `truncate-lines' is set TODO(package)
(defun maybe-show-horizontal-scroll-bar (symbol newval operation where)
  "Toggle the horizontal scroll bar based on `truncate-lines'.
If `truncate-lines' is non-nil in the current buffer, set the horizontal
scroll bar to be displayed as well.  If `truncate-lines' is nil, the
horizontal scroll bar will be based on frame parameters.  Also see
`toggle-horizontal-scroll-bar' and `horizontal-scroll-bar-mode'.

This function may be passed to `add-variable-watcher'.
Parameters SYMBOL, NEWVAL, OPERATION, and WHERE are as documented
there."
  (when (and (eq symbol 'truncate-lines)
             (eq operation 'set)
             (bufferp where))
    (setq horizontal-scroll-bar
          (if newval
              ;; Forced on
              'bottom
            ;; Fallback to default
            t))

    ;; Force any windows displaying this buffer to show a scroll bar
    (dolist (window (get-buffer-window-list where nil t))
      (set-window-buffer window where))))

;; Do same check as `horizontal-scroll-bars-available-p' does, but
;; ignore the current frame by not calling `display-graphic-p'. Just
;; because initial frame is non-graphical doesn't mean all frames are
;; not graphical.
(when (bound-and-true-p x-toolkit-scroll-bars)
  (add-variable-watcher 'truncate-lines #'maybe-show-horizontal-scroll-bar))

;;; Customizations for `ibuffer'

(add-hook 'ibuffer-mode-hook
          (defun my-ibuffer-hook ()
            (ibuffer-switch-to-saved-filter-groups "Home")
            (ibuffer-do-sort-by-alphabetic)
            (setf ibuffer-hidden-filter-groups '("Starred"))))
(setf ibuffer-show-empty-filter-groups nil
      ibuffer-saved-filter-groups '(("Home"
                                     ("Files" (visiting-file))
                                     ("Starred" (starred-name)))))

;; Make it mouse friendly
(with-eval-after-load 'ibuffer
  (keymap-set ibuffer-mode-filter-group-map
              "<mouse-1>" #'ibuffer-toggle-filter-group))
