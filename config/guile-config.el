;;; guile-config.el --- Settings for making Guile(s) workable -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Use geiser to make Guile development nicer
(use-package geiser-guile
  :straight t
  :defer t)

(provide 'guile-config)
;;; guile-config.el ends here
