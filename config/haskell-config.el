;;; haskell-config.el --- Provides and configures packages for writing Haskell
;;; Commentary:
;;; Code:

(use-package haskell-mode
  :ensure t
  :defer t)
(add-hook 'haskell-mode-hook
		  (lambda ()
			(haskell-doc-mode) ;; Displays the type signature for a function
			(turn-on-haskell-indent) ;; Use smart indentation for Haskell buffers/files
			))

;; Sub-mode for Haskell-mode
(use-package ghc
  :ensure t
  :defer t)



;; Interface to Stack Haskell development tool
(use-package hasky-stack
  :ensure t
  :defer t)

;; Lookup Haskell documentation
(use-package ghc-imported-from
  :ensure t
  :defer t)

;; flycheck stuff for Haskell
(use-package flycheck-haskell
  :ensure t
  :defer t)

;; Yasnippet snippets for Haskell
(use-package haskell-snippets
  :ensure t
  :defer t)

(provide 'haskell-config)
;;; haskell-config.el ends here
