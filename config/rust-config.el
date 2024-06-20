;;; rust-config.el --- Handles everything needed for Rust source code development -*- lexical-binding: t -*-
;;; Commentary:
;;
;; In order to make this work, you will need to install some Rust crates through
;; cargo. These are: rustfmt and racer.
;; rustfmt formats Rust code according to the Rust community style guidelines.
;;
;;; Code:

(require 'treesit-config)
(require 'lsp-config)

;; rust-mode is minimal, but relies on an LSP to provide good information.
;; Fortunately, I will always have rust-analyzer present in any Rust project I
;; work on, so I am fine there.
(use-package rust-mode
  :ensure t
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode)))

(provide 'rust-config)
;;; rust-config.el ends here
