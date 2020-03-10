;;; rust-config.el --- Handles everything needed for Rust source code development
;;; Commentary:
;;
;; In order to make this work, you will need to install some Rust crates through
;; cargo. These are: rustfmt and racer.
;; rustfmt formats Rust code according to the Rust community style guidelines.
;; racer allows for easier code navigation, but requires the entire Rust source
;;  code to work. It can be gotten with: git clone git@github.com:rust-lang/rust.git
;;
;;; Code:

(require 'magit)

;; rust-mode is developed by The Rust Language makers, but not actively maintained
;; (use-package rust-mode)

;; Therefore, I will use the rustic package for Rust development instead
;; This is a fork of rust-mode that is actively maintained.
;; Right now, it is generally better than rust-mode for everything.
(use-package rustic) ;; Development environment for Rust

(setq rustic-lsp-server 'rust-analyzer)
(setq rustic-lsp-client 'lsp-mode)

(add-hook 'rustic-mode-hook
		  (lambda ()
			;; In rustic-mode buffers, C-c <tab> will format the entire buffer's code
			(local-set-key (kbd "C-c <tab>") #'rustic-format-buffer)))

;; (require 'company-mode)
;; Racer is used for code completion and source code navigation
;; It requires that company-mode be loaded into Emacs
(use-package racer
  :after company-mode)

(setq racer-cmd "~/.cargo/bin/racer") ;; Binary path for rustup

;; (add-hook 'rust-mode-hook #'racer-mode)
;; (add-hook 'racer-mode-hook #'eldoc-mode)
;; (add-hook 'racer-mode-hook #'company-mode)

;; (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

;; Cargo is a package that allows us to control Rust's cargo from Emacs
(use-package cargo)

(provide 'rust-config)
;;; rust-config.el ends here
