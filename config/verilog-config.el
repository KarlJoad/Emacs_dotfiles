;;; verilog-config.el --- This file provides my personal information
;;; Commentary:
;;; Code:

(use-package verilog-mode)

(autoload 'verilog-mode "verilog-mode" "Verilog mode" t)
(add-to-list 'auto-mode-alist '("\\.[ds]?va?h?\\'" . verilog-mode))

(provide 'verilog-config)
;;; verilog-config.el ends here
