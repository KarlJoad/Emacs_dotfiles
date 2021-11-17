;;; common-lisp-config.el --- Handles everything needed for Common Lisp development
;;; Commentary:
;;
;;
;;; Code:

(require 'magit)

;; Get Yasnippets for Common Lisp
(use-package common-lisp-snippets)

;; Superior Lisp Interaction Mode for Emacs
;; IDE for Common Lisp in Emacs
(use-package slime)

;; Add company-mode completion for Common Lisp
(use-package slime-company)

;; Add ANSI collors to SLIME REPL output
(use-package slime-repl-ansi-color)

(provide 'common-lisp-config)
;;; common-lisp-config.el ends here
