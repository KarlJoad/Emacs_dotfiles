;;;; This file provides my personal changes to how I want org-babel to behave
(provide 'org-config)

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
