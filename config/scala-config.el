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

(provide 'scala-config)
;;; scala-config.el ends here
