;;; config-lang-config.el --- Allow Emacs to handle different configuration formats -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;;
;;; Configure YAML for non-Tree-Sitter enabled systems
;;;

(use-package yaml-mode
  :ensure t
  :defer t)


;;;
;;; Now configure YAML using tree-sitter tools
;;;

(use-package yaml-ts-mode
  :ensure nil ; built-in
  :defer t)


(provide 'config-lang-config)
;;; config-lang-config.el ends here
