;;; verilog-config.el --- This file provides my personal information
;;; Commentary:
;;; Code:

(use-package verilog-mode
  :straight (:type built-in)
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

(use-package verilog-ts-mode
  :straight t
  :defer t)

(provide 'verilog-config)
;;; verilog-config.el ends here
