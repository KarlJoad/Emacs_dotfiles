;;; auctex-latexmk-config.el --- Provides and makes my personal changes to the auctex-latex package
;;; Commentary:
;;; Code:

(use-package auctex-latexmk)

(setq auctex-latexmk-inherit-TeX-mode-pdf t) ;; Have LaTeXMK inherit the PDF flat from AucTeX
(setq TeX-file-line-error nil) ;; This ensures full log files are written
;; This is a workaround for MikTeX: https://sourceforge.net/p/miktex/bugs/2310/

(provide 'auctex-latexmk-config)
;;; auctex-latexmk-config.el ends here
