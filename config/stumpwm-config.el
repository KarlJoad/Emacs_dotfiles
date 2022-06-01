;;; stumpwm-config.el --- Handles everything needed for Common Lisp development
;;; Commentary:
;;
;;
;;; Code:

(require 'magit)
(require 'company)
(require 'yasnippet)
(require 'yasnippet-snippets)
(require 'lispy-config)
(require 'common-lisp-config)

;; Get Yasnippets for StumpWM specifically
(use-package stumpwm-mode)

;; To access most of StumpWM's internals from Sly, start with `(in-package :stumpwm)'.

(provide 'stumpwm-config)
;;; stumpwm-config.el ends here
