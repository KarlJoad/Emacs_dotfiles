;;; eshell-config.el --- Settings for making Eshell(s) workable
;;; Commentary:
;;; Code:

(use-package eshell
  :straight (:type built-in)
  :ensure t
  :defer t
  :bind (("s-s" . #'eshell)))

(provide 'eshell-config)
;;; eshell-config.el ends here
