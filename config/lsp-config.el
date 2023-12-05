;;; lsp-config --- Settings for lsp-mode
;;; Commentary:
;;; Code:


(use-package lsp-mode
  :straight t
  :defer t
  :ensure t
  :hook ((lsp-mode . lsp-diagnostics-mode)
         (lsp-mode . (lambda () (lsp-completion-mode 1)))
         (c-ts-mode . lsp-deferred)
         (c++-ts-mode . lsp-deferred))
  :init
  (setq lsp-use-plists t)
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-keep-workspace-alive nil)
  (lsp-idle-delay 0.5)
  ;; core
  (lsp-enable-xref t)
  ;; Disable LSP from configuring everything itself. I must configure LSP manually.
  (lsp-auto-configure nil)
  (lsp-eldoc-enable-hover nil)
  (lsp-enable-dap-auto-configure nil)
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)
  (lsp-enable-imenu nil)
  ;; LSP should not ask me to download LSP servers.
  (lsp-enable-suggest-server-download nil)
  ;; Prevent LSP from providing highlighting, since we use tree-sitter-based or
  ;; Emacs' built-in regexp-based major modes.
  (lsp-enable-indentation nil)
  (lsp-enable-links nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-enable-text-document-color nil)
  ;; completion
  (lsp-completion-enable t)
  (lsp-completion-enable-additional-text-edit nil)
  (lsp-enable-snippet nil)
  (lsp-completion-show-kind nil)
  ;; headerline
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-headerline-breadcrumb-enable-diagnostics nil)
  (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
  (lsp-headerline-breadcrumb-icons-enable nil)
  ;; modeline
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-modeline-workspace-status-enable nil)
  (lsp-signature-doc-lines 1)
  ;; lens
  (lsp-lens-enable nil)
  ;; semantic
  (lsp-semantic-tokens-enable nil))

;; Register clangd as an LSP backend for c-ts-mode & c++-ts-mode
(with-eval-after-load 'lsp-mode
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "clangd")
                    :activation-fn (lsp-activate-on "c")
                    :server-id 'clangd))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "clangd")
                    :activation-fn (lsp-activate-on "cpp")
                    :server-id 'clangd)))

;; Fancy sideline, Popup documentation, VS Code-like peek UI
(use-package lsp-ui
  :straight t
  :ensure t
  :defer t
  :commands lsp-ui-mode)

(add-hook 'lsp-mode-hook #'lsp-ui-mode)

(provide 'lsp-config)
;;; lsp-config.el ends here
