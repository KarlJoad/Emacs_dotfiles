;;; haskell-config.el --- Provides and configures packages for writing Haskell
;;; Commentary:
;;; Code:

(use-package haskell-mode)

;; Sub-mode for Haskell-mode
(use-package ghc
  :require 'haskell-mode)

;; flycheck stuff for Haskell
(use-package flycheck-haskell)

;; Yasnippet snippets for Haskell
(use-package haskell-snippets)

;; Interface to Stack Haskell development tool
(use-package hasky-sack)

;; Lookup Haskell documentation
(use-package ghc-imported-from)

(provide 'haskell-config.el)
;;; haskell-config.el ends here
