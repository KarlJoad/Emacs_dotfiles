;;; package-config-el --- Lists packages to load into Emacs
;;; Commentary:
;;; Code:
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
;; (use-package neotree)
(use-package treemacs
  :ensure t
  :defer t)

;; Spell-checking and syntax checking
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; Provide tags through gtags
(use-package gtags)

;; Use a project controller
(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

;; Give myself a git wrapper
(use-package magit)

;; Use a package that provides many QoL things for editing LaTeX/TeX documents
(use-package auctex
  :defer t
  :ensure t)

(use-package reftex ;; Require reftex package for LaTeX support
  :defer t)

;; Use a package that integrates auctex and latexmk for longer/bigger compiles
;(use-package auctex-latexmk)

;; Load a mode to edit markdown files with
(use-package markdown-mode
  :defer t)

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
(setq yas-global-mode 1) ; Make sure yasnippet is almost always present
; Snippets are expanded by typing enough of it in, and them pressing "<tab>"

;; Provides a good starting set of yasnippet snippets to use for most major modes
(use-package yasnippet-snippets
  :after yasnippet
  :ensure t
  :config (yasnippet-snippets-initialize))

;; This package replaces the Doc-view mode for PDFs. We'll see if it works
(use-package pdf-tools)

;; This package minimizes bullets that are used in Org-mode
(use-package org-bullets
  :init
  (add-hook 'org-mode-hook 'org-bullets-mode))
;; Syntax highlighting in source blocks
(setq org-src-fontify-natively t)
;; Make TAB act as if it were issued in a buffer of the language's major mode
(setq org-src-tab-acts-natively t)

;; Company gives us an engine for some auto-completion for things we provide a backend for
(use-package company
  :ensure t
  :defer t
  :init
  (global-company-mode)
  :config
  (setq company-idle-delay 0)
  (define-key company-active-map (kbd "<escape>") 'company-abort)
  (define-key company-active-map [tab] 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

;; Company-AUCTeX provides the backend for Company
;;(use-package company-auctex)
;;(company-auctex-init)

;; Company-math
(use-package company-math)

;; Nix stuff for editing *.nix files (NixOS, Nix Packages, etc.)
;; But only if the system is a GNU/Linux system, because Nix only supports those
(when (equal system-type 'gnu/linux)
  ;; Each of these is deferred because I won't necessarily edit Nix files on every
  ;; GNU/Linux computer that I use
  (use-package nix-mode ;; Major mode for editing *.nix files
    :defer t)
  (use-package json-mode ;; nix-mode needs json-mode for some reason
    :defer t)
  (use-package nixos-options ;; Options for the Nixos .nix files
    :defer t)
  (use-package company-nixos-options ;; Provide a company backend for nixos-options
    :defer t)
  )

(provide 'package-config)
;;; package-config.el ends here
