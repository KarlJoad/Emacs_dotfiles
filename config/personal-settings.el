;;;; This file provides my personal tweaks to Emacs
(provide 'personal-settings)

;;;; Remove scroll bar at side
(scroll-bar-mode -1)

;;;; Skip the "Welcome" Page
;(setq inhibit-startup-message t)

;;;; Turn on Line numbering
(global-display-line-numbers-mode) ; Show line numbers everywhere
(global-hl-line-mode 1) ; Have line with my cursor highlighted

(show-paren-mode) ; Emphasize Parentheses
(setq blink-matching-paren nil) ; But don't let them blink

(setq auto-save-default t) ; Allow the #auto-save# files. They are removed upon buffer save
(setq make-backup-files nil) ; Disable backup~ files

(setq visible-bell nil) ; Disable the visual bell
(setq ring-bell-function 'ignore) ; Don't make a ding when failing command

;;; Can now open recently opened files
(require 'recentf)
(recentf-mode 1) ; Turn on recentf
(setq recentf-max-saved-items 50) ; Maximum number of buffers to remember

;;; Restore opened files from last session
;(desktop-save-mode 1) ; Commented out only while debugging my init files
