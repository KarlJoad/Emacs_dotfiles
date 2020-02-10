;;; personal-settings.el --- Settings for making Emacs mine
;;; Commentary:
;;; Code:

;; Make Emacs Start Full-Screen
(when (equal system-type 'gnu/linux)
  (add-hook 'emacs-startup-hook 'toggle-frame-fullscreen) ;; When on GNU/Linux, make Emacs fullscreen
  )
(when (equal system-type 'windows-nt)
  (add-hook 'emacs-startup-hook 'toggle-frame-maximized) ;; When on Windows/DOS, make emacs maximized window
  )

;;;; Skip the "Welcome" Page
;;(setq inhibit-startup-message t)

;; Remove scroll bar at side, when running in a GUI instance
(when (display-graphic-p)
  (scroll-bar-mode -1)
  (menu-bar-mode 1) ;; Keep the top menu-bar, with the drop-down menus
  (tool-bar-mode -1) ;; But get rid of the big icon tool-bar below it.
  ;;;; Turn on Line numbering
  (global-display-line-numbers-mode) ;; Show line numbers everywhere
  (setq column-number-mode 1) ;; Turn on column numbers in ALL major modes
  (global-hl-line-mode 1)) ;; Have line with my cursor highlighted


(show-paren-mode) ;; Emphasize MATCHING Parentheses
(setq blink-matching-paren nil) ;; But don't let them blink
(use-package rainbow-delimiters) ;; Color nested parentheses/brackets/braces successively

(setq auto-save-default t) ;; Allow the #auto-save# files. They are removed upon buffer save anyways
(setq make-backup-files nil) ;; Disable backup~ files

(setq visible-bell nil) ;; Disable the visual bell
(setq ring-bell-function 'ignore) ;; Don't make a ding when failing command

;; Auto refresh buffers
(setq global-auto-revert-mode 1)
;; Also refresh dired, but quietly
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Show keystrokes in progress more quickly than default
(setq echo-keystrokes 0.75)

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

;; Transparently open compressed files
(auto-compression-mode t)

;; Try to use UTF-8 for everything
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Sentences DO NOT need 2 spaced to end.
(set-default 'sentence-end-double-space nil)

;; Can now open recently opened files
;; This is done with C-x f
(require 'recentf)
(recentf-mode 1) ;; Turn on recentf
(setq recentf-max-saved-items 50) ;; Maximum number of buffers to remember

;;; Restore opened files from last session
;;(desktop-save-mode 1) ;; Commented out only while debugging my init files

;; Set my prefered font
(if (equal system-type 'windows-nt)
    (add-to-list 'default-frame-alist
		 '(font . "Courier New-11"))) ;; In this case, 11pt Courier New

(provide 'personal-settings)
;;; personal-settings.el ends here
