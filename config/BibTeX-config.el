;;; BibTeX-config.el --- This file provides my changes to Emacs' default BibTeX major mode
;;; Commentary:
;;
;; By default, Emacs comes with a BibTeX major mode.
;; However, it also defaults to the BibTeX citation database.
;; BibTeX is old and relatively obsolete, so I generally use BibLaTeX instead.
;; To go with BibLaTeX, I use Biber as the backend citation and cross-referencing manager.
;;
;;; Code:

(defvar my-default-bibtex-dialect "biblatex"
  "By default, I like to use BibLaTeX, so I want to make sure I always use that.")

(bibtex-mode)

(add-hook 'bibtex-mode-hook
		  (lambda ()
			"Setup BibTeX-mode for me, but only when I open a *.bib file. Because not all globally exported variables work all the time."
			(setq bibtex-dialect 'biblatex)
			(setq bibtex-maintain-sorted-entries t)))

(provide 'BibTeX-config)
;;; BibTeX-config.el ends here
