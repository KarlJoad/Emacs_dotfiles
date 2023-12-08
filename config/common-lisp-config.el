;;; common-lisp-config.el --- Handles everything needed for Common Lisp development
;;; Commentary:
;;
;;
;;; Code:

(require 'magit-config)
(require 'company-config)
(require 'yasnippet-config)
(require 'lispy-config)

(setq inferior-lisp-program "sbcl")

(use-package slime
  :straight t
  :defer t)

(use-package sly
  :straight t
  :defer t
  :after (slime))

(paredit-mode)

(provide 'common-lisp-config)
;;; common-lisp-config.el ends here
