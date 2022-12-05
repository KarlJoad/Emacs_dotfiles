;;; tree-sitter-config.el --- Configure tree-sitter for code development
;;; Commentary:
;;; Code:

(use-package tree-sitter)

;; Core tree-sitter APIs
(use-package tsc)

(use-package tree-sitter-indent)

(use-package tree-sitter-ispell)

(use-package tree-sitter-langs)

;; Bring paredit-like functionality to every programming language!
(use-package tree-edit)

(provide 'tree-sitter-config)
;;; tree-sitter-config.el ends here
