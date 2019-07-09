;;;; This file provides my changes to keybindings and commands while in the major mode Org-mode
(provide 'org-mode-config)

;;; Org-mode Keybindings (Agenda)
(global-set-key (kbd "C-c a") 'org-agenda) ; "C-c a" opens the Agenda Buffer to choose where to go
(global-set-key (kbd "C-c l") 'org-store-link) ; "C-c l" stores a hyperlink to the cursor's current position in the current org-mode document
; (global-set-key (kbd "C-c c") 'org-capture) ; "C-c c" will let me select a template and file the new information

;; Make org-babel do stuff with  source code blocks in Org-mode
;; But only after there is an org file loaded up, otherwise, do nothing
(with-eval-after-load "org"
  (setq org-src-fontify-natively t) ;; Use syntax highliting in source blocks while editing
  (setq org-src-tab-acts-natively t) ;; Make TAB act as if it were issued natively in that language's major mode
  (setq org-src-window-setup 'current-window) ;; When C-c ' a code block, use same window
  (setq org-confirm-babel-evaluate nil) ;; Don't ask before evaluating code blocks
  (when (equal system-type 'windows-nt)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((C . t)
       (emacs-lisp . t)
       (gnuplot . t)
       (haskell . nil) ;; nil to not load haskell
       (python . t)
       (ruby . nil) ;; nil to not load ruby
       (sql . t)
       (sqlite . t)))) ;; End of Windows Setup
  (when (equal system-type 'gnu/linux)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((C . t)
       (emacs-lisp . t)
       (gnuplot . t)
       (haskell . nil) ;; nil to not load haskell
       (python . t)
       (ruby . nil) ;; nil to not load ruby
       (sh . t)
       (sql . t)
       (sqlite . t)))) ;; End of Linux Setup
  ) ;; End of Org-mode source code blocks setup
