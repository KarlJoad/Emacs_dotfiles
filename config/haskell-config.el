;;; haskell-config.el --- Provides and configures packages for writing Haskell -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package haskell-mode
  :ensure t
  :defer t
  :hook ((haskell-mode . (lambda ()
                             ;; Display function type signature
                             (haskell-doc-mode)
                             ;; Smart indentation for Haskell buffers/files
	                           (turn-on-haskell-indent)))))

(use-package dante
  :ensure t
  :defer t
  :after (haskell-mode)
  :commands 'dante-mode
  :hook ((haskell-mode . flycheck-mode)
         (haskell-mode . dante-mode)))

;; flycheck stuff for Haskell
(use-package flycheck-haskell
  :defer t)

(provide 'haskell-config)
;;; haskell-config.el ends here
