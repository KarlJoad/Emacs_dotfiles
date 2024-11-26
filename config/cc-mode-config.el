;;; cc-mode-config.el --- Config for C/C++ editing -*- lexical-binding: t -*-
					;-*-Emacs-Lisp-*-
;;; Commentary:
;;; Code:

(require 'treesit-config)
(require 'lsp-config)

;; Available C style:
;; “gnu”: The default style for GNU projects
;; “k&r”: What Kernighan and Ritchie, the authors of C used in their book
;; “bsd”: What BSD developers use, aka “Allman style” after Eric Allman.
;; “whitesmith”: Popularized by the examples that came with Whitesmiths C, an early commercial C compiler.
;; “stroustrup”: What Stroustrup, the author of C++ used in his book
;; “ellemtel”: Popular C++ coding standards as defined by “Programming in C++, Rules and Recommendations,” Erik Nyquist and Mats Henricson, Ellemtel
;; “linux”: What the Linux developers use for kernel development
;; “python”: What Python developers use for extension modules
;; “java”: The default style for java-mode (see below)
;; “user”: When you want to define your own style
(setq c-default-style "stroustrup")

(use-package cc-mode
  :ensure nil ; built-in
  :defer t
  :bind (:map c-mode-map
         ("C-c C-c" . compile)
         :map c++-mode-map
         ("C-c C-c" . compile)))

;; setup GDB
(setq gdb-many-windows t ;; use gdb-many-windows by default
      gdb-show-main t) ;; Non-nil means display source file containing the main routine at startup

(provide 'cc-mode-config)
;;; cc-mode-config.el ends here
