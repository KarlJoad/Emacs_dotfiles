;;; rust-config.el --- Handles everything needed for Rust source code development
;;; Commentary:
;;; Code:

(require 'magit)

(use-package rust-mode)

(use-package rustic) ;; Development environment for Rust

(setq rustic-lsp-server 'rust-analyzer)
(setq rustic-lsp-client 'lsp-mode)

(add-hook 'rustic-mode-hook
		  (lambda ()
			(local-set-key (kbd "C-c <tab>") #'rustic-format-buffer)))

;; (require company-mode)
;; Racer is used for code completion and source code navigation
(use-package racer)

(setq racer-cmd "~/.cargo/bin/racer") ;; Binary path for rustup

;; (add-hook 'rust-mode-hook #'racer-mode)
;; (add-hook 'racer-mode-hook #'eldoc-mode)
;; (add-hook 'racer-mode-hook #'company-mode)

;; (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

;; Cargo is a package that allows us to control Rust's cargo from Emacs
(use-package cargo)

(provide 'rust-config)
;;; rust-config.el ends here
