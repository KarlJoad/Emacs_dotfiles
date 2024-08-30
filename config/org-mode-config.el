;;; org-mode-config.el --- Provides and configures org-mode -*- lexical-binding: t -*-
;;; Commentary:
;;
;; This file provides my changes to keybindings and commands while in the major mode Org-mode
;;
;;; Code:

(use-package org
  :ensure nil ;; built-in
  :defer t
  :bind (;; These keybindings are set without needing an org file, because they should always be available.
	       ("C-c a" . 'org-agenda) ;; "C-c a" opens the Agenda Buffer to choose where to go
	       ("C-c l" . 'org-store-link) ;; "C-c l" stores a hyperlink to the cursor's current position in the current Org-mode document
	       ("C-c c" . 'org-capture) ;; "C-c c" will let me select a template and file the new information
	       )
  :config
  ;; Only load org-babel languages after loading org-mode, which we do lazily.
  ;; This is done lazily because it is time-consuming to do this during startup.
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
  ;; Enable line-wrapping in org-mode.
  (visual-line-mode 1)

  ;; Advise org-agenda-goto-today to behave how I prefer
  ;; NOTE: The syntax is:
  ;; (define-advice fn-to-advise (WHERE (args ...) advice-name)
  ;;   "Documentation String"
  ;;   body body1 ...)
  (define-advice org-agenda-goto-today (:before () org-agenda-refresh-before-goto-today)
    "Refresh all Org files that build the agenda before jumping to today."
    (message "Refreshing all Org-agenda files")
    (org-agenda-redo-all))
  (define-advice org-agenda-goto-today (:after () org-recenter-today-frame-top)
    "Recenter today to the top of the buffer/frame in org-mode's agenda."
    (recenter-top-bottom 'top))
  :custom
  ;; Use major-mode specific syntax highliting in source blocks while editing
  (org-src-fontify-natively t)
  ;; DO NOT put 2 leader spaces in source code.
  ;; Prevents issues with white-space sensitive languages
  (org-src-preserve-indentation t)
  ;; Make TAB act as if it were issued natively in that language's major mode
  (org-src-tab-acts-natively t)
  ;; When C-c ' a code block, use same window as org file
  (org-src-window-setup 'current-window)
  ;; Don't ask before evaluating code blocks
  (org-confirm-babel-evaluate nil)
  ;; Ensures that when tasks marked done, they also take the time that happened
  (org-log-done-with-time t)
  ;; Ensure subtasks in list are marked DONE before allowing parent to be DONE
  (org-enforce-todo-dependencies t)
  ;; Ensure checkboxes in list are marked DONE before allowing parent to be DONE
  (org-enforce-todo-checkbox-dependencies t)
  ;; Add ALL .org files in Agenda to the Agenda's file list
  (org-agenda-files '("~/Agenda/"))
  ;; Include American holidays on the Org-Agenda
  (org-agenda-include-diary t)
  ;; In the calendar to select days, highlight the ones that are American holidays.
  (calendar-mark-holidays-flag t))

;; This package minimizes bullets that are used in Org-mode
(use-package org-bullets
  :ensure t
  :hook ((org-mode . org-bullets-mode)))

;; Make sure we update Emacs' built-in emacsql
(use-package emacsql
  :ensure t
  :defer t)

;; emacsql-sqlite-builtin is a faster alternative than the separately-compiled
;; sqlite binary that org-roam used to use. This also means that we do not need
;; to compile emacsqlite, so no compiler, and thus, no need to have Guix build
;; it for us.
(use-package emacsql-sqlite-builtin
  :ensure t
  :defer t)

(use-package org-roam
  :ensure t
  :defer t
  :after cl-lib
  :custom (org-roam-directory (file-truename "~/OrgRoamNotes/"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n t" . karljoad/org-roam-node-find-by-tag)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today)
         :map org-mode-map
         ("M-," . org-mark-ring-goto))
  :init
  (require 'cl-lib)
  ;; TODO: Get list of possible completions from org-roam database, so the user
  ;; has an easier time selecting tags.
  (defun karljoad/org-roam-node-find-by-tag (tag)
    "Find and open an org-roam node based on the specified TAG."
    (interactive "sEnter Tag: ")
    (org-roam-node-find nil nil
                        (lambda (node) (karljoad/org-roam-node-has-tag node tag))
                        ;; PRED below gets used as a sorting function in the
                        ;; completion.
                        nil))
  (defun karljoad/org-roam-node-has-tag (node tag)
    "Filter function to check if the given NODE has the specified TAG."
    ;; Set operations are required because even though we can enter tags with
    ;; spaces through the minibuffer, org-roam v2 only uses org-mode's notion of
    ;; tags, which is controlled by the org-tag-re regexp. Notably, this means
    ;; that spaces are not allowed in tags. So if I want to filter by tags, and I
    ;; enter a space into the tag filter function, we should split it up so that
    ;; it can kind of match what org-tag-re expectes, and therefore what org-roam
    ;; actually uses and stores.
    (consp
     (cl-intersection (string-split tag)
                      (org-roam-node-tags node)
                      :test #'equal)))
  :config
  (progn
    (org-roam-db-autosync-mode)
    (org-roam-setup)
    (setq-local completion-ignore-case t))
  :custom
  (org-roam-v2-ack t))

(provide 'org-mode-config)
;;; org-mode-config.el ends here
