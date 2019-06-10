;;;; This file lists packages to use in Emacs, but does not configure them
;;;; Package configuration is done in each of the package-config.el files
(provide 'package-config)

;;; Ensure packages are always new and always loaded
(when (not (package-installed-p 'use-package)) ; When use-package is not installed,
  (package-refresh-contents)                   ; refresh package repositories,
  (package-install 'use-package))              ; and install use-package
(require 'use-package-ensure) ; Make sure use-package is loaded, and ready to compile stuff
(setq use-package-always-ensure t) ; Make sure that we can always ensure packages are ready
(use-package auto-compile ; If we just downloaded a package, then compile it too
  :config (auto-compile-on-load-mode)) ; Automatically compile packages on first load

;;; Packages that I am using
;; An interactive filetree on the side
(use-package neotree)

;; Give myself a git wrapper
(use-package magit)

;; Use a package that provides many QoL things for editing LaTeX/TeX documents
(use-package auctex
  :defer t
  :ensure t)

(use-package reftex) ; Require reftex package for LaTeX support

;; Use a package that integrates auctex and latexmk for longer/bigger compiles
;(use-package auctex-latexmk)

;; Load a mode to edit markdown files with
(use-package markdown-mode)

;; Load a snippet manager
(use-package yasnippet
  :ensure t
  :defer t
  :config
  (yas-reload-all)
  (setq yas-snippet-dirs '("~/.emacs.d/snippets")) ; Location to find MORE snippets
  (setq tab-always-indent 'complete) ; Tabs indent
  (setq yas-prompt-functions '(yas-completing-prompt ; List of prompts that yasnippet can go through
			       yas-ido-prompt
			       yas-dropdown-prompt))
  (define-key yas-minor-mode-map (kbd "<escape>") 'yas-exit-snippet)) ; If there was a menu for selecting snippets, provide an escape route
(yas-global-mode 1) ; Make sure yasnippet is almost always present
; Snippets are expanded by typing enough of it in, and them pressing "<tab>"

;; Provides a good starting set of yasnippet snippets to use for most major modes
(use-package yasnippet-snippets
  :after yasnippet
  :ensure t
  :config (yasnippet-snippets-initialize))

;; This package replaces the Doc-view mode for PDFs. We'll see if it works
(use-package pdf-tools)
