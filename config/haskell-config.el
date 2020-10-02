;;; haskell-config.el --- Provides and configures packages for writing Haskell
;;; Commentary:
;;; Code:

(use-package haskell-mode
  :defer t)
(add-hook 'haskell-mode-hook
	  (lambda ()
	    (haskell-doc-mode) ;; Displays the type signature for a function
	    (turn-on-haskell-indent) ;; Use smart indentation for Haskell buffers/files
	    ))

(use-package company-ghc
  :ensure t
  :defer t)

(use-package company-ghci
  :ensure t
  :defer t)

;; Sub-mode for Haskell-mode
(use-package ghc
  :defer t)

(use-package attrap
  :defer t)

(use-package dante
  :defer t
  :after haskell-mode
  :commands 'dante-mode
  :init
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  (add-hook 'haskell-mode-hook 'dante-mode))

;; Interface to Stack Haskell development tool
(use-package hasky-stack
  :defer t)

;; Lookup Haskell documentation
(use-package ghc-imported-from
  :defer t)

;; flycheck stuff for Haskell
(use-package flycheck-haskell
  :defer t)

;; Yasnippet snippets for Haskell
(use-package haskell-snippets
  :defer t)

(provide 'haskell-config)
;;; haskell-config.el ends here
