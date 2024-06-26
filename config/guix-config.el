;;; guix-config.el --- This file configures Guix -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'functional-packaging-config)

(use-package guix
  :defer t
  :ensure t)

(setq-default guix-state-directory "/var")


;;;
;;; Allow reading & checking Guix Debbugs instances from Emacs
;;;
(use-package debbugs
  :defer t
  :ensure t)

(provide 'guix-config)
;;; guix-config.el ends here
