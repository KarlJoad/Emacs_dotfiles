;;; eshell-config.el --- Settings for making Eshell(s) workable -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package eshell
  :ensure nil ;; built-in
  :defer t
  :bind (("s-s" . #'eshell)))

(provide 'eshell-config)
;;; eshell-config.el ends here
