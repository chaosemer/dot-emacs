;;; init/newcomers.el --- Local copy of newcomers-presets theme  -*- lexical-binding: t; -*-

;;; Declarations:
(defvar compilation-scroll-output)
(defvar completion-eager-update)        ; Added in Emacs 31
(defvar completion-eager-display)       ; Added in Emacs 31
(defvar font-use-system-font)    ; Only exists in builds compiled with
                                 ; display support.
(defvar vc-find-revision-no-save)
(defvar vc-use-incoming-outgoing-prefixes)

;;; Code:

;; Appearance-related options
(setf font-use-system-font t
      frame-resize-pixelwise t
      window-resize-pixelwise t
      mode-line-compact 'long)

;; Mouse-related options
(context-menu-mode 1)
(setf save-interprogram-paste-before-kill t
      ;;mouse-yank-at-point t - I don't like this setting
      ;;pixel-scroll-mode t - I use pixel-scroll-precision-mode instead
      mouse-drag-and-drop-region t
      mouse-drag-and-drop-region-cross-program t)
;; TODO(emacs31) Function only available on Emacs 31
(when (fboundp 'global-xref-mouse-mode)
  (global-xref-mouse-mode 1))

;; Persistence-related options
(savehist-mode 1)
(save-place-mode 1)
(recentf-mode 1)

;; Editing-related options
(electric-pair-mode 1)
(repeat-mode 1)
;(delete-selection-mode 1) - I use cua-mode instead
(editorconfig-mode 1)
(setf (default-value 'indent-tabs-mode) nil
      ;;imenu-auto-rescan t - I don't use imenu
      ;;view-read-only t - I don't like this setting
      )
(column-number-mode 1)

;; Directory management-related options
(setf dired-auto-revert-buffer t
      ;;dired-mouse-drag-files t - I don't like this setting
      shell-command-prompt-show-cwd t)

;; File-related options
;;(etags-regen-mode 1) - I don't like this setting
;;(vc-auto-revert-mode 1) - I don't like this setting
;;(setf vc-deduce-backend-nonvc-modes t) - I don't like this setting
;;(setf vc-save-some-buffers-on-revert t) - I don't like this setting
(setf vc-find-revision-no-save t
      ;;vc-follow-symlinks t - I don't like this setting
      vc-use-incoming-outgoing-prefixes t)

;; Completion-related options
;; TODO(emacs31): Many of these are only available in Emacs 31
(if (string-version-lessp emacs-version "31.1")
    (fido-mode 1)
  (setf minibuffer-visible-completions t
        completions-detailed t
        completions-group t
        ;;completion-auto-select 'second-tab - TODO: do I want this?
        completion-eager-update t
        completion-styles '(basic emacs22 flex))
  ;;(global-completion-preview-mode 1) - I don't like this setting
  (setf tab-always-indent 'complete)
  (which-key-mode 1)

  ;; Additional completion options I've customized:
  (setf completion-eager-display t
        completion-ignore-case t
        read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completions-max-height 20
        completion-auto-help 'always
        completion-show-help nil
        completions-format 'one-column)

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

;; Package-related options
;; TODO(emacs31) Function only available on Emacs 31
(when (fboundp 'package-autosuggest-mode)
  (package-autosuggest-mode 1))
;;(setf package-menu-use-current-if-no-marks t) - TODO: Do I like this setting?

;; Frame- and window-related options
(setf frame-inhibit-implied-resize t)
;;(tab-bar-history-mode 1) - I don't like the tab bar
;; (setf tab-bar-show 0) - I don't like the tab bar

;; Programming-related options
(setf compilation-scroll-output 'first-error)
