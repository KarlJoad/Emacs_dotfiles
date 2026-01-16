;;; scheme-config.el --- Settings for making Scheme(s) workable -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'magit)
(require 'snippets-config)
(require 'lispy-config)

;; scheme-mode comes built into Emacs, so no need to fetch anything for that.

;; Use geiser to make Scheme development nicer
(use-package geiser
  :defer t
  :ensure t)

(provide 'scheme-config)
;;; scheme-config.el ends here
