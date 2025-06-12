;;; graph-config.el --- Configure textually-formatted graph editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package graphviz-dot-mode
  :ensure t
  :defer t)

(use-package mermaid-mode
  :ensure t
  :defer t)

(use-package mermaid-ts-mode
  :ensure t
  :defer t)

(provide 'graph-config)
;;; graph-config.el ends here
