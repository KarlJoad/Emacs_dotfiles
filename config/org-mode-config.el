;;;; This file provides my changes to keybindings and commands while in the major mode Org-mode
(provide 'org-mode-config)

;;; Org-mode Keybindings (Agenda)
;; These keybindings are set without needing an org file, because they should always be available.
(global-set-key (kbd "C-c a") 'org-agenda) ; "C-c a" opens the Agenda Buffer to choose where to go
(global-set-key (kbd "C-c l") 'org-store-link) ; "C-c l" stores a hyperlink to the cursor's current position in the current Org-mode document
;; (global-set-key (kbd "C-c c") 'org-capture) ; "C-c c" will let me select a template and file the new information

;; Make org-babel do stuff with  source code blocks in Org-mode
;; But only after there is an org file loaded up, otherwise, do nothing
;; These are only loaded when an org file is first loaded because it is time-consuming to do this during Emacs startup
(with-eval-after-load "org"
  (setq org-src-fontify-natively t) ;; Use syntax highliting in source blocks while editing
  (setq org-src-preserve-indentation t) ;; DO NOT put the 2 leader spaces in source code. Prevents issues with white-space sensitive languages
  (setq org-src-tab-acts-natively t) ;; Make TAB act as if it were issued natively in that language's major mode
  (setq org-src-window-setup 'current-window) ;; When C-c ' a code block, use same window as org file
  (setq org-confirm-babel-evaluate nil) ;; Don't ask before evaluating code blocks
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
       (sql . t) ;; SQL blocks
       (sqlite . t) ;; SQLite blocks
       ))
  ;; Host OS-specific settings for Org-mode are below
  ;; If Windows specific things must be put together, follow the gnu/linux example
  (when (equal system-type 'gnu/linux)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((sh . t)) ;; Bash Shell mode for editing bash files in Linux
     )) ;; End of Linux Setup
  ) ;; End of Org-mode source code blocks setup
