;;; config-lang-config.el --- Allow Emacs to handle different configuration formats -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;;
;;; Configure YAML for non-Tree-Sitter enabled systems
;;;

(use-package yaml-mode
  :ensure t
  :defer t)


(provide 'config-lang-config)
;;; config-lang-config.el ends here
