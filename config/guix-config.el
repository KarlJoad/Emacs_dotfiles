;;; guix-config.el --- This file configures Guix -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'functional-packaging-config)

(use-package guix
  :ensure nil ; "Built-in" by Guix providing emacs-guix in Emacs metapackage
  :defer t)


;;;
;;; Allow reading & checking Guix Debbugs instances from Emacs
;;;
(use-package debbugs
  :ensure t
  :defer t)

(provide 'guix-config)
;;; guix-config.el ends here
