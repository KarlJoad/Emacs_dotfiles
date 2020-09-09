;;; BibTeX-config.el --- This file provides my changes to Emacs' default BibTeX major mode
;;; Commentary:
;;
;; By default, Emacs comes with a BibTeX major mode.
;; However, it also defaults to the BibTeX citation database.
;; BibTeX is old and relatively obsolete, so I generally use BibLaTeX instead.
;; To go with BibLaTeX, I use Biber as the backend citation and cross-referencing manager.
;;
;;; Code:

(defvar karljoad-default-bibtex-dialect 'biblatex
  "By default, I like to use BibLaTeX, so I want to make sure I always use that.")

;; Associate *.bib files with bibtex-mode.
;; This also applies to *.bib files that are written in BibLaTeX style as well.
(add-to-list 'auto-mode-alist '("\\.bib\\'" . bibtex-mode))

;; When bibtex-mode is called, we do not want to load it when we do not need to.
;; So, we let it autoload when we open a BibTeX/BibLaTeX file.
;; This also solves the problem of bibtex-parse-buffers-stealthily problem I was
;; having. It's likely this was a problem because I didn't have a *.bib file open
;; when Emacs thought I did.
(autoload 'bibtex-mode "Major mode for BibTeX and BibLaTeX files" t)

;; When creating/opening *.bib files for TeX and derivatives, use biblatex style
;; BibLaTeX is newer, so that should be the default anyways.
(setq bibtex-dialect karljoad-default-bibtex-dialect)

(add-hook 'bibtex-mode-hook
	  (lambda ()
	    "Setup BibTeX-mode for me, but only when I open a *.bib file. Because not all globally exported variables work all the time."
	    (bibtex-mode)
	    (setq bibtex-maintain-sorted-entries t)
	    (setq bibtex-parse-keys-timeout nil)))

(provide 'BibTeX-config)
;;; BibTeX-config.el ends here
