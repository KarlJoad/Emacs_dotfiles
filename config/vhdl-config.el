;;; vhdl-config.el --- This file configures Emacs for writing VHDL -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package vhdl-mode
  :ensure t ; Despite being built-in, use an update version
  :defer t)

(require 'treesit-config)
(use-package vhdl-ts-mode
  :ensure t
  :defer t)

(use-package vhdl-ext
  :ensure t
  :defer t
  :hook ((vhdl-mode . vhdl-ext-mode)
         (vhdl-ts-mode . vhdl-ext-mode))
  :init
  (setq vhdl-ext-feature-list
        '(font-lock
          xref
          capf
          hierarchy
          eglot
          flycheck
          beautify
          navigation
          template
          compilation
          imenu
          which-func
          hideshow
          time-stamp
          ports))
  :config
  (vhdl-ext-mode-setup))

(provide 'vhdl-config)
;;; vhdl-config.el ends here
