;;; guile-config.el --- Settings for making Guile(s) workable
;;; Commentary:
;;; Code:

;; Use geiser to make Guile development nicer
(use-package geiser-guile
  :defer t
  :straight t)

(provide 'guile-config)
;;; guile-config.el ends here
