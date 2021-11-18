;;; lsp-config --- Settings for lsp-mode
;;; Commentary:
;;; Code:

(setq lsp-keymap-prefix "C-M-l")

(use-package lsp-mode
  :straight t
  :defer t)

;; Fancy sideline, Popup documentation, VS Code-like peek UI
(use-package lsp-ui
  :straight t
  :defer t
  :commands lsp-ui-mode)

(add-hook 'lsp-mode-hook #'lsp-ui-mode)

(provide 'lsp-config)
;;; lsp-config ends here
