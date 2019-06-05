;;;; Karl's .emacs Init File
;;; Last Edited: 20190521

;;;; Make Emacs Start Full-Screen
(add-hook 'emacs-startup-hook 'toggle-frame-maximized)

;;;; Add Package Archives
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;;;; Ensure packages are always new and always loaded
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))
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

;;;; Remove scroll bar at side
(scroll-bar-mode -1)

;;;; Skip the "Welcome" Page
;(setq inhibit-startup-message t)

;;;; Turn on Line numbering
(global-display-line-numbers-mode)

;;;; Org-mode Things (Agenda)
(global-set-key (kbd "C-c a") 'org-agenda) ; "C-c a" opens the Agenda Buffer to choose where to go
(global-set-key (kbd "C-c l") 'org-store-link) ; "C-c l" stores a hyperlink to the cursor's current position in the current org-mode document
; (global-set-key (kbd "C-c c") 'org-capture) ; "C-c c" will let me select a template and file the new information

;;;; Neotree organizer in left area
(require 'neotree)
(global-set-key [f8] 'neotree-toggle) ; On F8 press, toggle the neotree project browser
;(setq neo-theme (if (display-graphic-p) 'nerd 'ascii))
(setq neo-theme (if (display-graphic-p) 'nerd 'ascii))

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

;;;; Set the Theme
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files nil)
 '(package-selected-packages
   (quote
    (use-package spacemacs-theme neotree markdown-mode magit auto-compile auctex all-the-icons))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
