;;; go-config.el --- Configure Emacs for the Go language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;;
;;; The normal regexp-based major modes for interacting with source-code Go.
;;;

(use-package go-mode
  :ensure t
  :defer t)

(use-package go-mod-mode
  :ensure (:host github :repo "zkry/go-mod-mode"
           :branch "master"
           :depth nil)
  :defer t
  :config
  (flycheck-go-mod-setup))


;;;
;;; Tree-sitter variants of source-code Go major-modes.
;;; The tree-sitter variants are built into Emacs already.
;;;

(use-package go-ts-mode
  :ensure nil ; built-in
  :defer t)

(use-package go-mod-ts-mode
  :ensure nil ; built-in
  :defer t)


;;;
;;; Other helpful packages that do not depend on the way fontification and
;;; syntax checking occurs in the major-mode.
;;;

(use-package go-eldoc
  :ensure t
  :defer t)

(provide 'go-config)
;;; go-config.el ends here
