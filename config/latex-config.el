;;; latex-config.el --- Provides and changes how I want to work with LaTeX
;;; Commentary:
;;
;; Auctex provides many QoL things for editing TeX/LaTeX documents
;;
;;; Code:

(use-package auctex
  :defer t
  :straight t)

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
                 TeX-run-TeX nil (latex-mode doctex-mode)
                 :help "Run LaTeX and put output in TeX_Output Directory"))
  (add-to-list 'TeX-command-list
				       '("LatexOutFolderShellEscape" "%`%l%(mode)%' -synctex=1 -interaction=nonstopmode -shell-escape -output-directory=./TeX_Output %T"
                 TeX-run-TeX nil (latex-mode doctex-mode)
                 :help "Run a shell-escaped version of LaTeX and put output in TeX_Output Directory"))
	(add-to-list 'TeX-command-list
		           '("LatexShellEscape" "%`%l%(mode)%' -synctex=1 -interaction=nonstopmode -shell-escape %T"
                 TeX-run-TeX nil (latex-mode doctex-mode)
                 :help "Run a shell-escaped version of LaTeX and put output in TeX_Output Directory"))
  (add-to-list 'TeX-command-list
		           '("BiberAuxDirectory" "biber --output-directory ./TeX_Output %s"
                 TeX-run-Biber nil t
                 :help "Run Biber where the .aux file is in the TeX_Output Directory"))
  (add-to-list 'TeX-command-list
		           '("Zathura View" "zathura ./TeX_Output/%o"
                 TeX-run-discard-or-function t t
                 :help "Run Zathura to view PDF"))
  (add-to-list 'TeX-command-list
		           '("Okular View" "okular ./TeX_Output/%o"
                 TeX-run-discard-or-function t t
                 :help "Run Okular to view PDF"))
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

;;; Extra code to further extend TeX/LaTeX/AucTeX
(require 'reftex-config) ;; RefTeX is part of Emacs, but it's getting its own config file
(require 'bibtex-config) ;; BibTeX is part of Emacs, but it's getting its own config file

(require 'company)
;; Since company needs a backend for its autocompletion, one is needed for TeX/LaTeX
;; Fortunately, auctex exposes enough for another package to make company work.
(use-package company-auctex)
(company-auctex-init)
;; Backend needed for company to provide autocompletion for TeX/LaTeX math.
(use-package company-math)

(provide 'latex-config)
;;; latex-config.el ends here
