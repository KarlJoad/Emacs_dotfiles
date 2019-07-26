;;; company-auctex-config.el --- Provides and configures Company's Auctex backend
;;; Commentary:
;;
;; Since company needs a backend for its autocompletion, one is needed for TeX/LaTeX
;; Fortunately, auctex exposes enough for another package to make company work.
;;
;;; Code:

(use-package company-auctex)
(company-auctex-init)

(provide 'company-auctex-config)
;;; company-auctex-config.el ends here
