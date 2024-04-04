;;; flycheck-config.el --- Provides and changes flycheck for my needs -*- lexical-binding: t -*-
;;; Commentary:
;;
;; flycheck requires a backend to run, just like company
;; flycheck is just the front end for the spell- and syntax-checking.
;; There MUST be a backend for flycheck to run off of.
;; C/C++ requires Clang
;; Elisp requires Emacs, etc.
;;
;;; Code:

;; Spell-checking and syntax checking
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(provide 'flycheck-config)
;;; flycheck-config.el ends here
