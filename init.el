;;;; Karl's .emacs Init File
;;; Last Edited: 20190521

;;;; Make Emacs Start Full-Screen
(add-hook 'emacs-startup-hook 'toggle-frame-maximized)

;;;; Tell Emacs where to look for my other config files
(setq user-emacs-directory (expand-file-name "~/.emacs.d/")) ; Directory where Emacs Files are located
(setq user-emacs-config-directory (concat user-emacs-directory "config/"))
(add-to-list 'load-path (concat user-emacs-directory "config/")) ; user-emacs-directory + "config/" to put the config directory in the load-path

;;;; Set up my personal information and my personal settings
(load "personal-info")
(load "personal-settings")

;;;; Add Package Archives
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;;;; Ensure packages are always new and always loaded
(when (not (package-installed-p 'use-package)) ; When use-package is not installed,
  (package-refresh-contents)                   ; refresh package repositories,
  (package-install 'use-package))              ; and install use-package
(require 'use-package-ensure) ; Make sure packages are loaded
(setq use-package-always-ensure t)
(use-package auto-compile
  :config (auto-compile-on-load-mode))
(use-package neotree)
(use-package magit)
(use-package auctex
  :defer t
  :ensure t)
(use-package reftex) ; Require reftex package for LaTeX support
(use-package auctex-latexmk)
(use-package markdown-mode)

;;; Loading in themes, to prevent the use of (custom-set-variables)
;;; This will make sure things are loaded in at the correct time
;;; Uncomment one of these lines, then change the load-theme line to the appropriate theme
;(use-package abyss-theme)
;(use-package cyberpunk-theme)
;(use-package doom-themes)
;(use-package spacemacs-theme ; Load the spacemacs themes up
;  :defer t
;  :init (load-theme 'spacemacs-dark t)) ; Load the dark theme up

;;;; Change Default custom-theme-load-path
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'tron t) ; Arguments: Themename No-confirm
(defun apply-theme-if-daemon ()
  "Apply the theme used above if emacs is evaluated with emacs --daemon, ensuring each subsequent frame is themed appropriately"
  (interactive)
  (load-theme 'tron t))

(if (daemonp)
    (add-hood 'after-make-frame-functions
	      (lambda (frame)
		(with-selected-frame frame (apply-theme-if-daemon))))
  (apply-theme-if-daemon))

;;;; Org-mode Things (Agenda)
(global-set-key (kbd "C-c a") 'org-agenda) ; "C-c a" opens the Agenda Buffer to choose where to go
(global-set-key (kbd "C-c l") 'org-store-link) ; "C-c l" stores a hyperlink to the cursor's current position in the current org-mode document
; (global-set-key (kbd "C-c c") 'org-capture) ; "C-c c" will let me select a template and file the new information

;;;; Neotree organizer in left area
(require 'neotree)
(global-set-key [f8] 'neotree-toggle) ; On F8 press, toggle the neotree project browser
;(setq neo-theme (if (display-graphic-p) 'nerd 'ascii))
(setq neo-theme (if (display-graphic-p) 'nerd 'ascii))

;;;; Load in Magit options
(load "personal-magit.el")

;;;; AucTeX options
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

;;; preview-latex Options
;(load "preview-latex.el" nil t t) ; noerror-nil, nomessage-t, nosuffix-t

;;; auctex-latexmk Options
(require 'auctex-latexmk)
(auctex-latexmk-setup)
(setq auctex-latexmk-inherit-TeX-mode-pdf t) ; Have LaTeXMK pass the -pdf flag when TeX-mode-pdf is active
(setq TeX-file-line-error nil) ; This is a workaround for MikTeX: https://sourceforge.net/p/miktex/bugs/2310/

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

;;;; Change Directory to where I want to work
(cd "c:/users/karl/documents/git")
