;;; scala-config.el --- This file provides changes to allow the writing of Scala
;;; Commentary:
;;; Code:

(require 'magit-config)

(use-package scala-mode
  :defer t
  :straight t)

(use-package sbt-mode
  :defer t
  :straight t)

;; LSP mode for handling scala with LSP
(use-package lsp-metals
  :ensure t
  :defer t
  :straight t
  :custom
  ;; Metals claims to support range formatting by default but it supports range
  ;; formatting of multiline strings only. You might want to disable it so that
  ;; emacs can use indentation provided by scala-mode.
  (lsp-metals-server-args '("-J-Dmetals.allow-multiline-string-formatting=off"))
  :hook (scala-mode . lsp))

(provide 'scala-config)
;;; scala-config.el ends here
