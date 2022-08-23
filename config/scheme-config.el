;;; scheme-config.el --- Settings for making Scheme(s) workable
;;; Commentary:
;;; Code:

(require 'magit)
(require 'company)
(require 'yasnippet)
(require 'yasnippet-snippets)
(require 'lispy-config)

;; scheme-mode comes built into Emacs, so no need to fetch anything for that.

;; Use geiser to make Scheme development nicer
(use-package geiser
  :defer t
  :straight t)

;; (use-package lsp-scheme
;;   :straight t
;;   :defer t)

(paredit-mode)

(provide 'scheme-config)
;;; scheme-config.el ends here
