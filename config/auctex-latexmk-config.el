;;;; These are my personal changes to the auctex-latex package
(provide 'personal-auctex-latex)

(require 'auctex-latexmk)
(auctex-latexmk-setup)
(setq auctex-latexmk-inherit-TeX-mode-pdf t) ; Have LaTeXMK pass the -pdf flag when TeX-mode-pdf is active
(setq TeX-file-line-error nil) ; This is a workaround for MikTeX: https://sourceforge.net/p/miktex/bugs/2310/
