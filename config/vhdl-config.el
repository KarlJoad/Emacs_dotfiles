;;; vhdl-config.el --- This file configures Emacs for writing VHDL
;;; Commentary:
;;; Code:

(use-package vhdl-mode
  :straight (:type built-in)
  :defer t)

(use-package vhdl-ts-mode
  :straight t
  :defer t)

(provide 'vhdl-config)
;;; vhdl-config.el ends here
