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
  ;; Using a hook is a work-around for setting these treesit variables AFTER the
  ;; major-mode is loaded. :config is run after the FEATURE is available, NOT
  ;; after the major-mode is loaded and ready.
  :hook
  (scala-ts-mode . (lambda ()
                     (setq-local treesit-font-lock-feature-list
                                   '((comment doc-comment definition)
                                     (keyword type)
                                     (variable function import literal)
                                     (operator interpolation extra)))
                     (setq-local treesit-font-lock-level 3)
                     (treesit-font-lock-recompute-features))))

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
