;;; lsp-mode-config --- Settings for lsp-mode
;;; Commentary:
;;; Code:

(setq lsp-keymap-prefix "C-M-l")

(use-package lsp-mode
  :straight t
  :defer t
  :hook (c++-mode . lsp)
  :commands lsp)

;;; Optional add-ins to lsp-mode

;; Fancy sideline, Popup documentation, VS Code-like peek UI
(use-package lsp-ui
  :straight t
  :defer t
  :commands lsp-ui-mode)

(add-hook 'lsp-mode-hook #'lsp-ui-mode)

;; Various tree based UI controls (symbols, errors overview, call hierarchy, etc.)
(use-package lsp-treemacs
  :straight t
  :defer t
  :commands lsp-treemacs-errors-list)

(provide 'lsp-mode-config)
;;; lsp-mode-config ends here
