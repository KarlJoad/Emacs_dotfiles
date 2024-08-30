;;; personal-settings.el --- Settings for making Emacs mine -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'os-detection)
;; Change the way an Emacs frame is drawn upon startup depending on OS.
(cond
 ;; On Windows GUI, make Emacs a maximized window
 ((equal system-type 'windows-nt)
  (progn
    (add-hook 'emacs-startup-hook 'toggle-frame-maximized)
    (add-to-list 'default-frame-alist '(fullscreen . fullboth))))
 ;; On Guix, I use StumpWM, use a maximized frame to NOT cover Stump's modeline
 ((karljoad/is-guix-system)
  (progn
    (add-hook 'emacs-startup-hook 'toggle-frame-maximized)
    (add-to-list 'default-frame-alist '(maximized . fullboth))))
 ;; Otherwise, on GNU/Linux/BSD/OSX, make Emacs fullscreen
 ((equal system-type 'gnu/linux)
  (progn
    (add-hook 'emacs-startup-hook 'toggle-frame-fullscreen)
    (add-to-list 'default-frame-alist '(fullscreen . fullboth)))))

;; Skip the "Welcome" Page
(setq inhibit-startup-message t)

(scroll-bar-mode -1) ;; Remove scroll bar at side
(menu-bar-mode 1) ;; Keep the top menu-bar, with the drop-down menus
(tool-bar-mode -1) ;; Remove big icon tool-bar below the menu-bar.
(tooltip-mode -1) ;; On clickable text, remove tooltip pop-up. Use minibuffer.

;; Unbind C-z from suspending the current Emacs frame.
;; This stops me from accidentally minimizing Emacs when running graphically.
;; You can still access this with C-x C-z (which is a default keybinding).
(keymap-global-unset "C-z")

;;;; Turn on Line numbering
(global-display-line-numbers-mode) ;; Show line numbers everywhere
(setq column-number-mode 1) ;; Turn on column numbers in ALL major modes
(global-hl-line-mode 1) ;; Have line with my cursor highlighted

;; Use special line-highlighting for line-oriented or text-oriented buffers.
;; In line-oriented buffers (mu4e, elfeed), highlights the point's current line
;; more heavily.
(use-package lin
  :ensure t
  :defer t
  :config
  (lin-global-mode)
  :custom
  (lin-face 'lin-mac))

;; Pulse the current line when performing certain actions in Emacs.
;; The functions that cause the pulse are in the `pulsar-pulse-functions' list.
(use-package pulsar
  :ensure t
  :config
  (pulsar-global-mode)
  :custom
  (pulsar-face 'pulsar-magenta)
  (pulsar-delay 0.05))

;; Parentheses/Brackets/Braces/Angles modifications
(show-paren-mode) ;; Emphasize MATCHING Parentheses/Brackets/Braces/Angles
(setq blink-matching-paren nil) ;; But don't let them blink
(require 'rainbow-delimiters-config) ;; Pull in rainbow-delimiters config
;(electric-pair-mode 1) ;; Emacs automatically inserts closing pair

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

;; Hide the long list of minor modes from the mode-line. The minions
;; package removes all the additional minor-mode names and their
;; information from the mode-line. If I have them all showing, the
;; modeline gets very busy, and very hard to read sometimes. So, I use
;; this package to remove them, leaving only the current major-mode
;; and a ;-) for the rest of the minor modes.
(use-package minions
  :ensure t
  :config
  (minions-mode 1))

;; Add highlighting for TODO/NOTE/FIXME strings in most buffers.
;; By default, it highlights TODO, FIXME, and NOTE.
;; You can also choose what words should be recognized and what color they should
;; be highlighted with my modifying the hl-todo-keyword-faces variable.
(use-package hl-todo
  :ensure (:depth nil)
  :bind (("C-c C-t p" . #'hl-todo-previous)
         ("C-c C-t n" . #'hl-todo-next))
  :config (global-hl-todo-mode))

;; (setq hl-todo-keyword-faces
;;       '(("TODO"   . "#FF0000")
;;         ("FIXME"  . "#FF0000")
;;         ("DEBUG"  . "#A020F0")
;;         ("GOTCHA" . "#FF4500")
;;         ("STUB"   . "#1E90FF")))


;; Change the title of the frame when opened in GUI mode.
(setq-default frame-title-format
							'("%b@" (:eval (or (file-remote-p default-directory 'host) system-name)) " - Emacs"))

;; Automatic file creation/manipulation/backups
;; I choose to remove the backup~ files because I don't want to have to add every one of those files
;; to the .gitignore for projects.
;; Besides, auto-saving happens frequently enough for it to not really matter.
(setq auto-save-default t) ;; Allow the #auto-save# files. They are removed upon buffer save anyways
(setq make-backup-files nil) ;; Disable backup~ files
(setq vc-follow-symlinks t) ;; Never ask whether or not to follow symlinks

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

;; ALWAYS ask for confirmation before exiting Emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; ANYTHING that should happen before saving ANY buffers should be put here.
(add-hook 'before-save-hook
          (lambda ()
            "Commands to execute before saving any buffer."
            (delete-trailing-whitespace)))

;; I want a keybinding to quickly revert buffers, since sometimes Magit doesn't do it.
(keymap-global-set "C-c g" 'revert-buffer)

;; Try to use UTF-8 for everything
(set-language-environment "UTF-8")
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8) ;; Catch-all

;; Sentences DO NOT need 2 spaces to end.
(set-default 'sentence-end-double-space nil)

;; Make sure tab-width is 4, not 8
(setq-default tab-width 4)

;;; CONSTANT DEFINITIONS
(defconst do-not-show-time 0
  "Do NOT show current time on the modeline.
Use with 'display-time-mode' function.")
(defconst show-time 1
  "Show current time in the modeline.
Use with 'display-time-mode' function.")

(defconst do-not-show-load nil
  "Do not display a load average on modeline.
Use with `display-time-default-load-average' variable.")
(defconst 1-minute-load 0
  "Put last 1 minute of load average on modeline.
Use with `display-time-default-load-average' variable.")
(defconst 5-minute-load 1
  "Put last 5 minutes of load average on modeline.
Use with `display-time-default-load-average' variable.")
(defconst 15-minute-load 2
  "Put last 15 minutes of load average on modeline.
Use with `display-time-default-load-average' variable.")

(defconst do-not-show-battery 0
  "Do NOT show battery information on the modeline.
Use with the 'display-battery-mode' function.")
(defconst show-battery 1
  "Show battery status in modeline.
Use with the 'display-battery-mode' function.")

(defconst karljoad/display-time-mode-line-format "%R %F"
  "Karl's preference on the time and date information to display on the modeline.
Displays the time in HH:MM format (24-hour), then the date in YYYY-MM-DD format.

Use with `display-time-format' variable.")
(defconst karljoad/battery-mode-line-format "[%p%%,%mMin]"
  "Karl's preference on the battery information to display on the modeline.
Displays an approximation of the current amount of battery left, as a
percentage, then the number of minutes left until the battery is emptied or
fully charged.

Use with `battery-mode-line-format' variable.")
;;; END OF CONSTANT DEFINITIONS

;; Show time, date, and system process load information in the modeline.
(display-time-mode show-time) ;; Show system time in buffer modeline.
;; (setq display-time-24hr-format t) ;; Show system time in 24-hour clock
;; (setq display-time-day-and-date t) ;; Show time AND date
(setq display-time-format karljoad/display-time-mode-line-format) ;; Karl's preferred display-time setup
(setq display-time-default-load-average 5-minute-load)

;; Show battery information in the modeline.
(display-battery-mode show-battery) ;; Show battery status info in buffer modeline.
(setq battery-mode-line-format karljoad/battery-mode-line-format) ;; Karl's preferred battery-display setup.

(setq-default tab-width 2) ; Default to indentation size of 2 spaces
(setq-default indent-tabs-mode nil) ; Use spaces instead of tabs

;; Scratch is a package that allows me to create a *scratch* buffer for any
;; major mode that I may be working in right now. By default, it opens a new
;; scratch buffer with the same name as the programming language I am currently
;; working in.
(use-package scratch
  :ensure t
  :bind (("C-c s" . #'scratch)))

(keymap-global-set "C-c w" 'whitespace-mode)


;; Set my preferred font style.
(defconst karljoad/preferred-font
  (cond
   ((equal system-type 'windows-nt) "Courier New-11") ;; In this case, 11pt Courier New
   ((equal system-type 'gnu/linux) "Iosevka Semibold-10.5")) ;"Fira Code Retina-11"
  "My (KarlJoad's) preferred font.
This needs to be a string that matches a font available on the system Emacs is
currently running on.")

;; NOTE: Emacs' :height face-attribute is in 1/10pt, so 105 = 10.5 point font
(defconst karljoad--default-font-height
  (cond
   ((equal system-type 'windows-nt) 110)
   ((equal system-type 'gnu/linux) 105))
  "The \"height\" of the default face (font) when Emacs starts.")

(add-to-list 'initial-frame-alist `(font . ,karljoad/preferred-font))
(add-to-list 'default-frame-alist `(font . ,karljoad/preferred-font))


;;; Registers & Bookmarks
;;; Registers are single-character named "boxes" to store any kind of
;;; information in Emacs, including locations of the point (cursor).
;;; Bookmarks are similar, but use full names instead.
;;; In addition, bookmarks are persistent across sessions, whereas
;;; registers MAY not be.

;; Immediately pop up the register preview when using register commands.
(setq-default register-preview-delay 0)

;; Make Emacs repeat the C-u C-SPC command (`set-mark-command') by
;; following it up with another C-SPC.  It is faster to type
;; C-u C-SPC, C-SPC, C-SPC, than C-u C-SPC, C-u C-SPC, C-u C-SPC...
(setq-default set-mark-command-repeat-pop t)

;; I want Emacs to write the list of bookmarks to the `bookmark-file'
;; as soon as I set a new bookmark.  The default behaviour of Emacs is
;; to write to the disk as a final step before closing Emacs.  Though
;; this can lead to data loss, such as in the case of a power failure.
;; Storing the data outright mitigates this problem.
(setq bookmark-save-flag 1)

;; If you are using the wonderful `consult' package, set up the
;; register preview facility with its more informative presentation:
(use-package consult
  :ensure t
  :defer t
  :custom
  (register-preview-function #'consult-register-format))


;;; Searching
(use-package ispell
  :ensure nil ; Use built-in
  :custom
  (isearch-lazy-count t)
  (isearch-count-prefix-format "(%s/%s) ")
  (isearch-count-prefix-format nil))


;;; Window navigation
;;; Windows contain buffers, and are the things you switch between.
;;; For example, C-x o runs the command (other-window) by default.
;;; Add keybindings for moving between windows in certain directions.
;;; These were originally set to S-direction.
(keymap-global-set "C-c C-b" #'windmove-left)
(keymap-global-set "C-c C-n" #'windmove-down)
(keymap-global-set "C-c C-p" #'windmove-up)
(keymap-global-set "C-c C-f" #'windmove-right)

(provide 'personal-settings)
;;; personal-settings.el ends here
