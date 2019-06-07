;;;; This file provides my personal changes to how I want AucTeX to behave
(provide 'personal-auctex)

;;; Start by setting the LaTeX command style
(setq LaTeX-command-style '(("" "%(PDF)%(latex) -synctex=1 -interaction=nonstopmode -aux-directory=./TeX_Aux_Files -output-directory=./TeX_Output %S%(PDFout)")))
					; synctex: Have Input and Output line up when viewed 
					; interaction: Have processor ignore many errors, so this can be automated
                                        ; aux-directory: Put auxiliary files in in ./TeX_Aux_Files directory relative to the master document
					; output-directory: Also puts output file in ./TeX_Output directory relative to the master document
					; From: https://tex.stackexchange.com/questions/157242/adding-an-option-to-the-pdflatex-call-from-auctex

(setq TeX-parse-self t) ; Parse multifile documents automagically
(setq TeX-show-compilation t) ; Always show compilation output
(setq TeX-command-default "pdflatex") ; Default compile to PDF
(setq LaTeX-biblatex-use-Biber t) ; Make biblatex use Biber automatically
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(setq reftex-plug-into-AUCTeX t)

;;; Set up the compilation options
(function (lambda()
	    '(TeX-command-list
	      (quote
	       (("TeX" "%(PDF)%(tex) %(file-line-error) %`%(extraopts) %S%(PDFout)%(mode)%' %t" TeX-run-TeX nil (plain-tex-mode texinfo-mode ams-tex-mode) :help "Run plain TeX")
                 ("LaTeX" "%`%l%(mode)%' %T" TeX-run-TeX nil
                 (latex-mode doctex-mode)
		 :help "Run LaTeX")
		 ("Makeinfo" "makeinfo %(extraopts) %t" TeX-run-compile nil
		  (texinfo-mode)
		  :help "Run Makeinfo with Info output")
		 ("Makeinfo HTML" "makeinfo %(extraopts) --html %t" TeX-run-compile nil
		  (texinfo-mode)
		  :help "Run Makeinfo with HTML output")
		 ("AmSTeX" "amstex %(PDFout) %`%(extraopts) %S%(mode)%' %t" TeX-run-TeX nil
		  (ams-tex-mode)
		  :help "Run AMSTeX")
		 ("ConTeXt" "%(cntxcom) --once --texutil %(extraopts) %(execopts)%t" TeX-run-TeX nil
		  (context-mode)
		  :help "Run ConTeXt once")
		 ("ConTeXt Full" "%(cntxcom) %(extraopts) %(execopts)%t" TeX-run-TeX nil
		  (context-mode)
		  :help "Run ConTeXt until completion")
		 ("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX")
		 ("Biber" "biber --input-directory ./TeX_Aux_Files --output-directory ./TeX_Aux_Files %s" TeX-run-Biber nil t :help "Run Biber with TeX_Aux_Files Directory")
		 ("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer")
		 ("Print" "%p" TeX-run-command t t :help "Print the file")
		 ("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command)
		 ("File" "%(o?)dvips %d -o %f " TeX-run-dvips t t :help "Generate PostScript file")
		 ("Dvips" "%(o?)dvips %d -o %f " TeX-run-dvips nil t :help "Convert DVI file to PostScript")
		 ("Dvipdfmx" "dvipdfmx %d" TeX-run-dvipdfmx nil t :help "Convert DVI file to PDF with dvipdfmx")
		 ("Ps2pdf" "ps2pdf %f" TeX-run-ps2pdf nil t :help "Convert PostScript file to PDF")
		 ("Glossaries" "makeglossaries %s" TeX-run-command nil t :help "Run makeglossaries to create glossary file")
		 ("Index" "makeindex %s" TeX-run-index nil t :help "Run makeindex to create index file")
		 ("upMendex" "upmendex %s" TeX-run-index t t :help "Run upmendex to create index file")
		 ("Xindy" "texindy %s" TeX-run-command nil t :help "Run xindy to create index file")
		 ("Check" "lacheck %s" TeX-run-compile nil
		  (latex-mode)
		  :help "Check LaTeX file for correctness")
		 ("ChkTeX" "chktex -v6 %s" TeX-run-compile nil
		  (latex-mode)
		  :help "Check LaTeX file for common mistakes")
		 ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document")
		 ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files")
		 ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files")
		 ("Other" "" TeX-run-command t t :help "Run an arbitrary command")
;		 ("Complete Build" "latexmk -e \"$pdflatex=q/pdflatex %%O %S %(mode) %%S/\" -e \"$biber=q/biber %%O --input-directory ./TeX_Aux_Files --output-directory ./TeX_Aux_Files %%B/\" -e \"$makeindex=q/makeindex %%O -o %%D %%S/\" -norc -gg -pdf %t" TeX-run-TeX nil (latex-mode) :help "Run Latexmk-pdfLaTeX")
		 )))))

;;; auctex-latexmk Options
(load "personal-auctex-latexmk.el")
