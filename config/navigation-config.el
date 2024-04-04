;;; navigation-config.el --- Ease navigation through Emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; Show information in minibuffer about function/variable and keybinding
(use-package marginalia
  :ensure t
  :defer nil
  :config (marginalia-mode)
  :bind (("M-A" . marginalia-cycle)))

(provide 'navigation-config)
;;; navigation-config.el ends here
