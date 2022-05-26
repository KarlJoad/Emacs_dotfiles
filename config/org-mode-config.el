;;; org-mode-config.el --- Provides and configures org-mode
;;; Commentary:
;;
;; This file provides my changes to keybindings and commands while in the major mode Org-mode
;;
;;; Code:
(use-package org
  :straight (:type built-in)
  :defer t
  :bind (;; These keybindings are set without needing an org file, because they should always be available.
	 ("C-c a" . 'org-agenda) ;; "C-c a" opens the Agenda Buffer to choose where to go
	 ("C-c l" . 'org-store-link) ;; "C-c l" stores a hyperlink to the cursor's current position in the current Org-mode document
	 ("C-c c" . 'org-capture) ;; "C-c c" will let me select a template and file the new information
	 )
  :config
  ;; Use major-mode specific syntax highliting in source blocks while editing
  (setq org-src-fontify-natively t)
  ;; DO NOT put 2 leader spaces in source code.
  ;; Prevents issues with white-space sensitive languages
  (setq org-src-preserve-indentation t)
  ;; Make TAB act as if it were issued natively in that language's major mode
  (setq org-src-tab-acts-natively t)
  ;; When C-c ' a code block, use same window as org file
  (setq org-src-window-setup 'current-window)
  ;; Don't ask before evaluating code blocks
  (setq org-confirm-babel-evaluate nil)
  ;; Ensures that when tasks marked done, they also take the time that happened
  (setq org-log-done-with-time t)
  ;; Ensure subtasks in list are marked DONE before allowing parent to be DONE
  (setq org-enforce-todo-dependencies t)
  ;; Ensure checkboxes in list are marked DONE before allowing parent to be DONE
  (setq org-enforce-todo-checkbox-dependencies t)
  ;; Add ALL .org files in Agenda to the Agenda's file list
  (setq org-agenda-files '("~/Agenda/"))
  ;; Include American holidays on the Org-Agenda
  (setq org-agenda-include-diary t)
  ;; In the calendar to select days, highlight the ones that are American holidays.
  (setq calendar-mark-holidays-flag t))

;; Make org-babel do stuff with source code blocks in Org-mode
;; But only after there is an org file loaded up, otherwise, do nothing
;; These are only loaded when an org file is first loaded because it is time-consuming to do this during Emacs startup
;;(with-eval-after-load "org"
(add-hook 'org-mode-hook
	  (lambda ()
	    "Setup settings for Org-mode specifically"
	    ;; Host agnostic settings for Org-mode
	    (org-babel-do-load-languages
	     'org-babel-load-languages
	     '((C . t) ;; Works on C, C++, and D source blocks
	       (emacs-lisp . t) ;; For ELisp blocks
	       (java . t) ;; Java Blocks
	       (gnuplot . nil) ;; nil to not load gnuplot
	       (haskell . nil) ;; nil to not load haskell
	       (matlab . nil) ;; nil to not load MATLAB stuff
	       (octave . t) ;; Octave code blocks (Octave is a GNU replacement for MATLAB)
	       (python . t) ;; Python blocks
	       (R . t) ;; R blocks (Not sure when I'll use this, but I will some day)
	       (ruby . nil) ;; nil to not load Ruby
	       ;; (rust . t) ;; Rust, provided by rust-mode, which I am not using.
	       ;; (rustic . t) ;; Rust which is not currently provided by the rustic package
	       ;; (sml . t) ;; SML, only supported if the ob-sml.el package is manually installed
	       (sql . t) ;; SQL blocks
	       (sqlite . t) ;; SQLite blocks
	       ))
	    ;; Host OS-specific settings for Org-mode are below
	    ;; If Windows specific things must be put together, follow the gnu/linux example
	    (when (equal system-type 'gnu/linux)
	      (org-babel-do-load-languages
	       'org-babel-load-languages
	       '((shell . t)) ;; Bash Shell mode for editing bash files in Linux
	       )) ;; End of Linux Setup
	    )) ;; End of Org-mode source code blocks setup

(defadvice org-agenda-goto-today (before org-agenda-refresh-before-goto-today ())
  "Refresh all Org files that build the agenda before jumping to today."
  (org-agenda-redo-all))

(defadvice org-agenda-goto-today (after org-recenter-today-frame-top ())
  "Recenter today to the top of the buffer/frame in org-mode's agenda."
  (recenter-top-bottom 'top))

;; This package minimizes bullets that are used in Org-mode
(use-package org-bullets
  :init
  (add-hook 'org-mode-hook 'org-bullets-mode))

(use-package org-roam
  :straight t
  :defer t
  :custom (org-roam-directory (file-truename "~/OrgRoamNotes/"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-db-autosync-mode)
  (org-roam-setup))

(setq org-roam-v2-ack t)

(provide 'org-mode-config)
;;; org-mode-config.el ends here
