;;; assembly-config.el --- This file configures various assembly languages modes
;;; Commentary:
;;; Code:

;; x86 and amd64 assembly major-mode
(use-package masm-mode
  :straight t
  :defer t)

(use-package nasm-mode
  :straight t
  :defer t)

;; MIPS assembly major-mode
(use-package mips-mode
  :straight t
  :defer t)

;; RISC-V assembly major-mode
(use-package riscv-mode
  :straight t
  :defer t)

(provide 'assembly-config)
;;; assembly-config.el ends here
