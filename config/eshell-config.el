;;; eshell-config.el --- Settings for making Eshell(s) workable -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package eshell
  :straight (:type built-in)
  :defer t
  :bind (("s-s" . #'eshell)))

(provide 'eshell-config)
;;; eshell-config.el ends here
