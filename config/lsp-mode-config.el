;;; lsp-mode-config --- Settings for lsp-mode
;;; Commentary:
;;; Code:
(use-package lsp-mode
  :ensure t
  :defer t
  :hook (c++-mode . lsp)
  :commands lsp)

;; Optional add-ins to lsp-mode
(use-package lsp-ui
  :ensure t
  :defer t
  :commands lsp-ui-mode)

(add-hook 'lsp-mode-hook #'lsp-ui-mode)

;; Various tree based UI controls (symbols, errors overview, call hierarchy, etc.)
(use-package lsp-treemacs
  :ensure t
  :defer t
  :commands lsp-treemacs-errors-list)

(provide 'lsp-mode-config)
;;; lsp-mode-config ends here
