;;;; This file provides my changes to keybindings and commands while in the major mode Org-mode
(provide 'org-mode-config)

;;; Org-mode Keybindings (Agenda)
(global-set-key (kbd "C-c a") 'org-agenda) ; "C-c a" opens the Agenda Buffer to choose where to go
(global-set-key (kbd "C-c l") 'org-store-link) ; "C-c l" stores a hyperlink to the cursor's current position in the current org-mode document
; (global-set-key (kbd "C-c c") 'org-capture) ; "C-c c" will let me select a template and file the new information

;; Make org-babel work with source code blocks in Org docs
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
     (sqlite . t))))

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
     (sqlite . t))))
