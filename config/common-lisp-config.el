;;; common-lisp-config.el --- Handles everything needed for Common Lisp development
;;; Commentary:
;;
;;
;;; Code:

(require 'magit)
(require 'company)
(require 'yasnippet)
(require 'yasnippet-snippets)
(require 'lispy-config)

;; Get Yasnippets for Common Lisp
(use-package common-lisp-snippets)

;; Superior Lisp Interaction Mode for Emacs
;; IDE for Common Lisp in Emacs
(use-package slime)

(setq slime-lisp-implementations
      '((sbcl ("sbcl"))))
(let ((slime-extra '(slime-fancy
                     slime-xref-browser
                     slime-quicklisp
                     slime-asdf
                     slime-indentation)))
  (when (ignore-errors (find-library-name "slime-company"))
    (add-to-list 'slime-extra 'slime-company))
  (define-key slime-editing-map (kbd "C-c l d") #'slime-documentation-lookup)
  (slime-setup slime-extra))

;; Add company-mode completion for Common Lisp
(use-package slime-company)

;; Add ANSI collors to SLIME REPL output
(use-package slime-repl-ansi-color)



(provide 'common-lisp-config)
;;; common-lisp-config.el ends here
