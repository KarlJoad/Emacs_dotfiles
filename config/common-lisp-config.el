;;; common-lisp-config.el --- Handles everything needed for Common Lisp development -*- lexical-binding: t -*-
;;; Commentary:
;;
;;
;;; Code:

(require 'magit-config)
(require 'company-config)
(require 'yasnippet-config)
(require 'lispy-config)

(setq inferior-lisp-program "sbcl")

(use-package sly
  :straight t
  :defer t)

(paredit-mode)

(provide 'common-lisp-config)
;;; common-lisp-config.el ends here
