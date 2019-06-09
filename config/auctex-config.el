;;;; This file provides my personal changes to how I want AucTeX to behave
(provide 'personal-auctex)

;;; Start by setting the LaTeX command style
;(setq LaTeX-command-style '(("" "%(PDF)%(latex) -synctex=1 -interaction=nonstopmode -aux-directory=./TeX_Aux_Files -output-directory=./TeX_Output %S%(PDFout)")))
					; synctex: Have Input and Output line up when viewed 
					; interaction: Have processor ignore many errors, so this can be automated
                                        ; aux-directory: Put auxiliary files in in ./TeX_Aux_Files directory relative to the master document
					; output-directory: Also puts output file in ./TeX_Output directory relative to the master document
					; From: https://tex.stackexchange.com/questions/157242/adding-an-option-to-the-pdflatex-call-from-auctex

(setq TeX-parse-self t) ; Parse multifile documents automagically
(setq TeX-show-compilation t) ; Always show compilation output
(setq TeX-global-PDF-mode t) ; Make the default TeX mode PDF mode
(setq TeX-command-default "pdflatex") ; Default compile to PDF
(setq LaTeX-biblatex-use-Biber t) ; Make biblatex use Biber automatically
(setq reftex-plug-into-AUCTeX t) ; Make reftex plug into AUCTeX true
(setq TeX-source-correlate-mode t) ; Correlate output files to input so we can easily navigate
(setq TeX-source-correlate-method 'synctex)
(setq TeX-source-correlate-start-server t)

(with-eval-after-load "latex"
  (when (equal system-type 'windows-nt)
    (setq TeX-view-program-list '(("Adobe Reader"
				   "\"c:/Program Files (x86)/Adobe/Acrobat Reader DC/Reader/AcroRd32.exe\" ./TeX_Output/%o")
				  ("SumatraPDF" "\"c:/Program Files/SumatraPDF/SumatraPDF.exe\" ./TeX_Output/%s.pdf")
				  ("Emacs Buffer" "\"c:/emacs-26.2-x86_64/bin/emacsclientw.exe -n\" ./TeX_Output/%o"))) ; %o is the output file
    (setq TeX-view-program-selection '(((output-dvi style-pstricks) "dvips and start")
				       (output-pdf "SumatraPDF")))
    )
  (when (equal system-type "gnu/linux")
    (setq TeX-view-program-list '(("Zathura" "")
				  ("Okular" "")))
    (setq TeX-view-program-selection '(((output-dvi style pstricks) "dvips and start")
				       (output-pdf "Zathura")))
    )
  (set-TeX-command-list) ; Sets up my TeX-command-list
  )

;;; Set up the compilation options
(defun set-TeX-command-list ()
  "Sets up the TeX-command-list for me"
  ; Command-list Format: Command Name, Command, How, Prompt, Modes, Help Info
  ;; (add-to-list 'TeX-command-list
  ;; 	       ("Latexmk" "latexmk %t" Tex-run-Tex nil (latex-mode) :help "Run Latexmk")) ; I don't have LaTeXmk installed on my systems, but may do later
  (add-to-list 'TeX-command-list
  	       '("LatexOutFolder" "%`%l%(mode)%' -synctex=1 -interaction=nonstopmode -aux-directory=./TeX_Aux_Files -output-directory=./TeX_Output %T" TeX-run-TeX nil (latex-mode doctex-mode) :help "Run LaTeX and put output in TeX_Output Directory"))
  (add-to-list 'TeX-command-list
  	       '("BiberAuxDirectory" "biber --input-directory ./TeX_Aux_Files --output-directory ./TeX_Aux_Files %s" TeX-run-Biber nil t :help "Run Biber with TeX_Aux_Files Directory"))
  (add-to-list 'TeX-command-list
	       '("IndexAuxDirectory" "makeindex %s" TeX-run-index nil t :help "Run makeindex to create index file in TeX_Aux_Files Directory"))
  (add-to-list 'TeX-command-list
	       '("GlossaryAuxDirectory" "makeglossaries %s" TeX-run-command nil t :help "Run makeglossaries to create glossary file in TeX_Aux_Files Directory"))
  (add-to-list 'TeX-command-list
	       '("Buffer View" "\"C:/emacs-26.2-x86_64/bin/emacsclientw.exe\" -n -e '(find-file-other-window ./TeX_Output/%o)"  TeX-run-discard-or-function t t :help "Open output PDF in Emacs Buffer"))
  (when (equal system-type 'windows-nt)
    (add-to-list 'TeX-command-list
		 '("Adobe View" "\"C:/Program Files (x86)/Adobe/Acrobat Reader DC/Reader/AcroRd32.exe\" ./TeX_Output/%o" TeX-run-discard-or-function t t :help "Run Adobe Acrobat Reader DC to View PDF")) ;%o is the output file's name and extension
    )
  )

;;; Add a way to open the output PDF in Emacs itself, like TeXStudio
;(setq TeX-output-view-style
 ;     (cons (list "^pdf$" "."
;		  "emacsclientw -n -e '(find-file-other-window \"/TeX_Output/%o\")'")
;	    TeX-output-view-style))
;(add-to-list 'TeX-view-program-list
;	     '(("Adobe Reader" ("\"C:/Program Files (x86)/Adobe/Acrobat Reader DC/Reader/AcroRd32.exe\" ./TeX_Output/%o"))
;	       ("Emacs PDF Viewer" ("\"C:/emacs-26.2-x86_64/bin/emacsclientw.exe\" -n -e" " ./TeX_Output/%o"))
;	       ))

;;; auctex-latexmk Options
;(load "auctex-latexmk-config")
