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
(use-package neotree)

(use-package magit)

(use-package auctex
  :defer t
  :ensure t)

(use-package reftex) ; Require reftex package for LaTeX support

(use-package auctex-latexmk)

(use-package markdown-mode)

(use-package yasnippet
  :ensure t
  :defer t
  :config
  (yas-reload-all)
  (setq yas-snippet-dirs '("~/emacs.d/snippets"))
  (setq tab-always-indent 'complete) ; Tabs indent
  (setq yas-prompt-functions '(yas-completing-prompt
			       yas-ido-prompt
			       yas-dropdown-prompt))
  (define-key yas-minor-mode-map (kbd "<escape>") 'yas-exit-snippet))

(use-package yasnippet-snippets
  :ensure t)
