;;; guix-config.el --- This file configures Guix
;;; Commentary:
;;; Code:

(require 'functional-packaging-config)

(use-package guix
  :defer t
  :straight t)

(setq-default guix-state-directory "/var")

(provide 'guix-config)
;;; guix-config.el ends here
