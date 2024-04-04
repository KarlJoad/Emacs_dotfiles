;;; assembly-config.el --- This file configures various assembly languages modes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; x86 and amd64 assembly major-mode
(use-package masm-mode
  :ensure t
  :defer t)

(use-package nasm-mode
  :ensure t
  :defer t)

;; MIPS assembly major-mode
(use-package mips-mode
  :ensure t
  :defer t)

;; RISC-V assembly major-mode
(use-package riscv-mode
  :ensure t
  :defer t)

(provide 'assembly-config)
;;; assembly-config.el ends here
