;;; vhdl-config.el --- This file configures Emacs for writing VHDL -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package vhdl-mode
  :straight (:type built-in)
  :defer t)

(require 'treesit-config)
(use-package vhdl-ts-mode
  :straight t
  :defer t)

(provide 'vhdl-config)
;;; vhdl-config.el ends here
