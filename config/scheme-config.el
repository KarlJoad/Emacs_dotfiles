;;; scheme-config.el --- Settings for making Scheme(s) workable -*- lexical-binding: t -*-
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

(paredit-mode)

(provide 'scheme-config)
;;; scheme-config.el ends here
