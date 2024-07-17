;;; scala-config.el --- This file provides changes to allow the writing of Scala -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'magit-config)
(require 'lsp-config)

(use-package scala-mode
  :ensure (scala-mode :inherit elpaca-menu-melpa)
  :defer t)

(use-package scala-ts-mode
  :ensure t
  :defer t
  :config
  ;; Tinkering with making Scala literals, functions, and vars font-lock the
  ;; same way as scala-mode. Involves tinkering with tree-sitter's
  ;; font-lock-feature-list.
  (progn
    (setq treesit-font-lock-feature-list
          '((comment doc-comment definition)
            (keyword type)
            (variable function import literal)
            (operator interpolation extra)))
    (setq treesit-font-lock-level 3)
    (treesit-font-lock-recompute-features)))

(use-package sbt-mode
  :ensure t
  :defer t)

;; LSP mode for handling scala with LSP
;; (use-package lsp-metals
;;   :ensure t
;;   :defer t
;;   :after (eglot scala-mode)
;;   :custom
;;   ;; Metals claims to support range formatting by default but it supports range
;;   ;; formatting of multiline strings only. You might want to disable it so that
;;   ;; emacs can use indentation provided by scala-mode.
;;   (lsp-metals-server-args '("-J-Dmetals.allow-multiline-string-formatting=off"))
;;   :hook (scala-mode . eglot-ensure))

(provide 'scala-config)
;;; scala-config.el ends here
