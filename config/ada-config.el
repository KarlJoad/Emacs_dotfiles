;;; ada-config.el --- This file configures Ada -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Provide the 2012 Revision of the Ada Reference Manual
(use-package ada-ref-man)


;;;
;;; Non-tree-sitter major-modes for working with Ada projects & files
;;;

(use-package ada-mode)

(use-package gpr-mode)


;;;
;;; Tree-sitter powered major-modes for working with Ada projects & files
;;;

(use-package ada-ts-mode)

(use-package gpr-ts-mode)

(provide 'ada-config)
;;; ada-config.el ends here
