;;; config-lang-config.el --- Allow Emacs to handle different configuration formats -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;;
;;; Configure YAML for non-Tree-Sitter enabled systems
;;;

(use-package yaml-mode
  :ensure t
  :defer t)

(use-package outline-indent
  :ensure t
  :defer t)


;;;
;;; Now configure YAML using tree-sitter tools
;;;

(use-package yaml-ts-mode
  :ensure nil ; built-in
  :defer t)

;; treesit-fold is intended to provide folding support for treesit.el that is
;; built-in since Emacs 29.1.
;; I am only ever going to use tree-sitter with Emacs >29.1, so I will always
;; choose this one.
(use-package treesit-fold
  :ensure t
  :defer t)


;;;
;;; Configure TOML
;;;

;; XXX: Emacs has a normal TOML mode built into it as a conf-mode derived
;; major-mode since 2017. I will probably not use any Emacsen older than this,
;; So I will not do any configuration here.

(use-package toml-ts-mode
  :ensure nil ; built-in
  :defer t)

(provide 'config-lang-config)
;;; config-lang-config.el ends here
