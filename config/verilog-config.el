;;; verilog-config.el --- This file configures Emacs to write -*- lexical-binding: t -*-
;;; Verilog/SystemVerilog
;;; Commentary:
;;; Code:

(use-package verilog-mode
  :ensure nil ;; built-in
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.[ds]?va?h?\\'" . verilog-mode))
  :custom
  (verilog-indent-level             3)
  (verilog-indent-level-module      3)
  (verilog-indent-level-declaration 3)
  (verilog-indent-level-behavioral  3)
  (verilog-indent-level-directive   1)
  (verilog-case-indent              2)
  (verilog-auto-newline             t)
  (verilog-auto-indent-on-newline   t)
  (verilog-tab-always-indent        t)
  (verilog-auto-endcomments         t)
  (verilog-minimum-comment-distance 40)
  (verilog-indent-begin-after-if    t)
  (verilog-auto-lineup              'declarations)
  (verilog-linter                   "my_lint_shell_command"))

(require 'treesit-config)
(use-package verilog-ts-mode
  :ensure t
  :defer t)

(provide 'verilog-config)
;;; verilog-config.el ends here
