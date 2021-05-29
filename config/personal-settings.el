;;; personal-settings.el --- Settings for making Emacs mine
;;; Commentary:
;;; Code:

;; Make Emacs Start Full-Screen
;; Except on Windows, where I think the window decorations are nice.
(if (equal system-type 'windows-nt) ;; ONLY when on Windows/GUI DOS
	(add-hook 'emacs-startup-hook 'toggle-frame-maximized) ;; Make Emacs a maximized window
  (add-hook 'emacs-startup-hook 'toggle-frame-fullscreen)) ;; Otherwise, on GNU/Linux/BSD/OSX, make Emacs fullscreen

(add-to-list 'default-frame-alist '(fullscreen . fullboth))

;; Skip the "Welcome" Page
(setq inhibit-startup-message t)

(scroll-bar-mode -1) ;; Remove scroll bar at side
(menu-bar-mode 1) ;; Keep the top menu-bar, with the drop-down menus
(tool-bar-mode -1) ;; Always get rid of the big icon tool-bar below the menu-bar.

;;;; Turn on Line numbering
(global-display-line-numbers-mode) ;; Show line numbers everywhere
(setq column-number-mode 1) ;; Turn on column numbers in ALL major modes
(global-hl-line-mode 1) ;; Have line with my cursor highlighted

;; Parentheses/Brackets/Braces/Angles modifications
(show-paren-mode) ;; Emphasize MATCHING Parentheses/Brackets/Braces/Angles
(setq blink-matching-paren nil) ;; But don't let them blink
(require 'rainbow-delimiters-config) ;; Require that we pull in the rainbow-delimiters config from here
;(electric-pair-mode 1) ;; Emacs automatically inserts closing pair

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

;; Hide the long list of minor modes from the mode-line.
(require 'minions-config)

;; Add highlighting for TODO/NOTE/FIXME strings in most buffers.
(require 'hl-todo-config)

;; Change the title of the frame when opened in GUI mode.
(setq-default frame-title-format
			  '("%b@" (:eval (or (file-remote-p default-directory 'host) system-name)) " - Emacs"))

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

;; Have Emacs always follow compilation outputs in the Compile buffers.
(setq compilation-scroll-output t)

;; Show keystrokes in progress more quickly than default
(setq echo-keystrokes 0.75)

;; ALWAYS as for confirmation before exiting Emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; ANYTHING that should happen before saving ANY buffers should be put here.
(add-hook 'before-save-hook
	  (lambda ()
	    "Commands to execute before saving any buffer."
	    (delete-trailing-whitespace)))

;; I want a keybinding to quickly revert buffers, since sometimes Magit doesn't do it.
(global-set-key (kbd "C-c g") 'revert-buffer)

;; Try to use UTF-8 for everything
(set-language-environment "UTF-8")
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8) ;; A catch-all for any locations a coding system might be where I have forgotten.

;; Sentences DO NOT need 2 spaces to end.
(set-default 'sentence-end-double-space nil)

;; Make sure tab-width is 4, not 8
(setq-default tab-width 4)

;;; CONSTANT DEFINITIONS
(defconst do-not-show-time 0
  "Do NOT show current time on the modeline. Use with 'display-time-mode' function.")
(defconst show-time 1
  "Show current time in the modeline. Use with 'display-time-mode' function.")

(defconst do-not-show-load nil
  "Do not display a load average on modeline. Use with display-time-default-load-average variable.")
(defconst 1-minute-load 0
  "Put last 1 minute of load average on modeline. Use with display-time-default-load-average variable.")
(defconst 5-minute-load 1
  "Put last 5 minutes of load average on modeline. Use with display-time-default-load-average variable.")
(defconst 15-minute-load 2
  "Put last 15 minutes of load average on modeline. Use with display-time-default-load-average variable.")

(defconst do-not-show-battery 0
  "Do NOT show battery information on the modeline. Use with the 'display-battery-mode' function.")
(defconst show-battery 1
  "Show battery status in modeline. Use with the 'display-battery-mode' function.")

(defconst karljoad-display-time-mode-line-format "%R %F"
  "Karl's preference on the time and date information to display on the modeline. Use with display-time-format variable.")
(defconst karljoad-battery-mode-line-format "[%p%%,%mMin]"
  "Karl's preference on the battery information to display on the modeline. Use with battery-mode-line-format variable.")
;;; END OF CONSTANT DEFINITIONS

;; Show time, date, and system process load information in the modeline.
(display-time-mode show-time) ;; Show system time in buffer modeline.
;; (setq display-time-24hr-format t) ;; Show system time in 24-hour clock
;; (setq display-time-day-and-date t) ;; Show time AND date
(setq display-time-format karljoad-display-time-mode-line-format) ;; Karl's preferred display-time setup
(setq display-time-default-load-average 5-minute-load)

;; Show battery information in the modeline.
(display-battery-mode show-battery) ;; Show battery status info in buffer modeline.
(setq battery-mode-line-format karljoad-battery-mode-line-format) ;; Karl's preferred battery-display setup.

;; Can now open recently opened files
;; When Recentf mode is enabled, a "Open Recent" submenu is displayed in the "File" menu.
(require 'recentf)
(recentf-mode 1) ;; Turn on recentf
(setq recentf-max-saved-items 25) ;; Maximum number of buffers to remember

;;; Restore opened files from last session
;;(desktop-save-mode 1) ;; Commented out only while debugging my init files

;; Set my prefered font
(when (equal system-type 'windows-nt)
  (add-to-list 'default-frame-alist
	       '(font . "Courier New-11"))) ;; In this case, 11pt Courier New

;; Scratch is a package that allows me to create a *scratch* buffer for any
;; major mode that I may be working in right now. By default, it opens a new
;; scratch buffer with the same name as the programming language I am currently
;; working in.
(use-package scratch)
(global-set-key (kbd "C-c s") #'scratch)

(global-set-key (kbd "C-c w") 'whitespace-mode)

(provide 'personal-settings)
;;; personal-settings.el ends here
