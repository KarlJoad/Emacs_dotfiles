;;; tex-config.el --- Provides and changes how I want to work with TeX-based docs -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Auctex provides many QoL things for editing TeX/LaTeX documents
;; However, it also defaults to the BibTeX citation database. BibTeX
;; is fairly old, so I prefer to use BibLaTeX instead. To go along
;; with BibLaTeX, I use Biber as the backend citation and
;; cross-referencing manager.
;;
;;; Code:

;; Emacs ships with some default TeX/LaTeX support.
(require 'tex-mode)

;;; 
;;; RefTeX
(use-package reftex
  :straight (:type built-in)
  :defer t
  :after (tex-mode)
  ;; Make sure that reftex gets loaded when AucTeX gets loaded, i.e. when LaTeX file is opened
  :hook (latex-mode . #'turn-on-reftex)
  :bind (:map latex-mode-map
              ;; Scan the whole document for new labels/citations
              ("C-c r" . #'reftex-parse-all))
  :custom
  ;; Make RefTeX play nice with AucTeX
  (reftex-plug-into-AUCTeX t)
  ;; When parsing very large documents, we might not want to reparse every file
  (reftex-enable-partial-scans t)
  ;; Set default citation style for RefTeX to use
  (reftex-cite-format 'biblatex)
  ;; Set a default style to present possible citation matches
  (reftex-sort-bibtex-matches 'author))

;;; 
;;; BibTeX
(defvar karljoad/default-bibtex-dialect 'biblatex
  "By default, I like to use BibLaTeX, so I want to make sure I always use that.")

(use-package bibtex
  :straight (:type built-in) ;; BibTeX comes with Emacs
  :defer t
  :hook (bibtex-mode . (lambda () (setq bibtex-dialect karljoad/default-bibtex-dialect)))
  :custom
  ;; Default to newer BibLaTeX style
  (bibtex-dialect karljoad/default-bibtex-dialect)
  (bibtex-maintain-sorted-entries t)
  (bibtex-parse-keys-timeout nil))

;; Associate *.bib files with bibtex-mode.
;; This also applies to *.bib files that are written in BibLaTeX style as well.
(add-to-list 'auto-mode-alist '("\\.bib\\'" . bibtex-mode))

;;; 
;;; AucTeX

(use-package auctex
  :straight t
  :defer t
  :after (reftex bibtex))

(setq TeX-parse-self t) ;; Parse multifile documents automagically
(setq TeX-auto-save t) ;; Enables parsing upon saving the document
(setq TeX-show-compilation t) ;; Always show compilation output
(setq TeX-global-PDF-mode t) ;; Make the default TeX mode PDF mode
(setq TeX-command-default "pdflatex") ;; Default compile to PDF
(setq LaTeX-biblatex-use-Biber t) ;; Make biblatex use Biber automatically
(setq TeX-electric-sub-and-superscript t) ;; Inserts {} automaticly on _ and ^
(setq TeX-source-correlate-mode t) ;; Correlate output to input so we can easily navigate
(setq TeX-source-correlate-method 'synctex)
(setq TeX-source-correlate-start-server t)

;;; Set up the compilation options
(defun karljoad/set-TeX-command-list ()
  "Set up the TeX-command-list for me."
  ;; %l adds the current LaTeX-command-style to the command line
  ;; Usually, this is "pdflatex/xelatex/lualatex -file-line-error ..."
  ;; %(mode) adds -interaction=nonstopmode to command line if TeX-interactive-mode
  ;; %T adds currently active master document to command line, with quotes
  (add-to-list 'TeX-command-list
	             '("IndexAuxDirectory" "makeindex %s"
                 TeX-run-index nil t
                 :help "Run makeindex to create index file in TeX_Aux_Files Directory"))
  (add-to-list 'TeX-command-list
	             '("GlossaryAuxDirectory" "makeglossaries %s"
                 TeX-run-command nil t
                 :help "Run makeglossaries to create glossary file in TeX_Aux_Files Directory"))
  (add-to-list 'TeX-command-list
		           '("LatexOutFolder"
                 "%`%l%(mode)%' -synctex=1 -interaction=nonstopmode -output-directory=./TeX_Output %T"
                 TeX-run-TeX nil (plain-tex-mode latex-mode doctex-mode ams-tex-mode)
                 :help "Run LaTeX and put output in TeX_Output Directory"))
  (add-to-list 'TeX-command-list
				       '("LatexOutFolderShellEscape" "%`%l%(mode)%' -synctex=1 -interaction=nonstopmode -shell-escape -output-directory=./TeX_Output %T"
                 TeX-run-TeX nil (plain-tex-mode latex-mode doctex-mode ams-tex-mode)
                 :help "Run a shell-escaped version of LaTeX and put output in TeX_Output Directory"))
	(add-to-list 'TeX-command-list
		           '("LatexShellEscape" "%`%l%(mode)%' -synctex=1 -interaction=nonstopmode -shell-escape %T"
                 TeX-run-TeX nil (plain-tex-mode latex-mode doctex-mode ams-tex-mode)
                 :help "Run a shell-escaped version of LaTeX"))
  (add-to-list 'TeX-command-list
		           '("latexmk" "latexmk"
                 TeX-run-TeX nil (latex-mode ams-tex-mode)
                 :help "Run latexmk to compile the document"))
  (add-to-list 'TeX-command-list
		           '("BiberAuxDirectory" "biber --output-directory ./TeX_Output %s"
                 TeX-run-Biber nil t
                 :help "Run Biber where the .aux file is in the TeX_Output Directory"))
  (add-to-list 'TeX-command-list
		           '("Zathura View" "zathura ./%o"
                 TeX-run-discard-or-function t t
                 :help "Run Zathura to view PDF"))
  (add-to-list 'TeX-command-list
		           '("Okular View" "okular ./%o"
                 TeX-run-discard-or-function t t
                 :help "Run Okular to view PDF"))
  (add-to-list 'TeX-command-list
		           '("Okular View Out Folder" "okular ./TeX_Output/%o"
                 TeX-run-discard-or-function t t
                 :help "Run Okular to view PDF in ./TeX_Output/ directory"))
  (add-to-list 'TeX-command-list
		           '("Buffer View" "emacsclient -n -c ./TeX_Output/%o"
                 TeX-run-discard-or-function t t
                 :help "View PDF in an Emacs Buffer")))

(with-eval-after-load "latex"
    (setq TeX-view-program-list '(("Zathura" "zathura ./TeX_Output/%o")
				  ("Okular" "okular ./TeX_Output/%o")
				  ("Emacs Buffer" "emacsclient -n -e ./TeX_Output/%o")))
    (setq TeX-view-program-selection '(((output-dvi style pstricks) "dvips and start")
				       (output-pdf "Zathura")))
  (karljoad/set-TeX-command-list)) ;; Calls the function that sets up my TeX-command-list

;;; Apply latex-mode to TikZ pictures
(setq auto-mode-alist
      (append '(("\\.tikz\\'" . latex-mode))
	      auto-mode-alist))

(require 'company-config)
;; Since company needs a backend for its autocompletion, one is needed for TeX/LaTeX
;; Fortunately, auctex exposes enough for another package to make company work.
(use-package company-auctex)
(company-auctex-init)
;; Backend needed for company to provide autocompletion for TeX/LaTeX math.
(use-package company-math)

(provide 'tex-config)
;;; tex-config.el ends here
