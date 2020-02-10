;;; personal-settings.el --- Settings for making Emacs mine
;;; Commentary:
;;; Code:

;; Make Emacs Start Full-Screen
;; Except on Windows, where I think the window decorations are nice.
(if (equal system-type 'windows-nt) ;; ONLY when on Windows/GUI DOS
	(add-hook 'emacs-startup-hook 'toggle-frame-maximized) ;; Make Emacs a maximized window
  (add-hook 'emacs-startup-hook 'toggle-frame-fullscreen) ;; Otherwise, on GNU/Linux/BSD/OSX, make Emacs fullscreen
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

;; Parentheses/Brackets/Braces/Angles modifications
(show-paren-mode) ;; Emphasize MATCHING Parentheses/Brackets/Braces/Angles
(setq blink-matching-paren nil) ;; But don't let them blink
(require 'rainbow-delimiters-config) ;; Require that we pull in the rainbow-delimiters config from here

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

;; Hide the long list of minor modes from the mode-line.
(require 'minions-config)

;; Automatic file creation/manipulation/backups
;; I choose to remove the backup~ files because I don't want to have to add every one of those files
;; to the .gitignore for projects.
;; Besides, auto-saving happens frequently enough for it to not really matter.
(setq auto-save-default t) ;; Allow the #auto-save# files. They are removed upon buffer save anyways
(setq make-backup-files nil) ;; Disable backup~ files

;; Disable some minor disturbances that I find quite annoying.
(setq visible-bell nil) ;; Disable the visual bell
(setq ring-bell-function 'ignore) ;; Don't make a ding when failing command

;; NOTE: Emacs calls refreshing a buffer a revert.
;; Unless you have modifications in memory that are not saved to the disk, then you will be fine.
(setq global-auto-revert-mode t) ;; Auto refresh buffers
;; Also refresh dired, but quietly
(setq global-auto-revert-non-file-buffers t) ;; Allow buffers not attached to a file to refresh themselves
;; Usually, a message is generated everytime a buffer is reverted and placed in the *Messages* buffer.
(setq auto-revert-verbose nil) ;; But not right now.
(auto-compression-mode t) ;; Transparently open compressed files

;; Show keystrokes in progress more quickly than default
(setq echo-keystrokes 0.75)

;; ALWAYS as for confirmation before exiting Emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; ANYTHING that should happen before saving ANY buffers should be put here.
(add-hook 'before-save-hook
		  (lambda ()
			"Commands to execute before saving any buffer."
			(delete-trailing-whitespace)))

;; Try to use UTF-8 for everything
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8) ;; A catch-all for any locations a coding system might be where I have forgotten.

;; Sentences DO NOT need 2 spaces to end.
(set-default 'sentence-end-double-space nil) ;; Having spaces end in 2 spaces is stupid.

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
