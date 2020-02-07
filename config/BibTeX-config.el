;;; BibTeX-config.el --- This file provides my changes to Emacs' default BibTeX major mode
;;; Commentary:
;;
;; By default, Emacs comes with a BibTeX major mode.
;; However, it also defaults to the BibTeX citation database.
;; BibTeX is old and relatively obsolete, so I generally use BibLaTeX instead.
;; To go with BibLaTeX, I use Biber as the backend citation and cross-referencing manager.
;;
;;; Code:

(setq bibtex-set-dialect 'biblatex)

(provide 'BibTeX-config)
;;; BibTeX-config.el ends here
