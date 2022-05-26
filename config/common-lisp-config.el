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

(setq inferior-lisp-program "sbcl")

(use-package sly)

(paredit-mode)

(provide 'common-lisp-config)
;;; common-lisp-config.el ends here
