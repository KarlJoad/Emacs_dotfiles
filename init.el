;;; init.el --- Karl's .emacs Init File
					;-*-Emacs-Lisp-*-
;;; Commentary:
;;
;; Karl's Emacs init file, in all of it's weird glory
;;
;;; Code:

;; Make sure Emacs loads up newer config files, even if they aren't compiled
(setq load-prefer-newer t)

;; Don't necessarily start packages at startup
;;(setq package-enable-at-startup nil)

;; Tell Emacs where to look for my other config files
(setq user-emacs-config-directory (concat user-emacs-directory "config/"))
(setq custom-file (concat user-emacs-config-directory "customize.el")) ;; File for things written by the "customize" stuff in emacs
;;(load-file custom-file) ;; Prevent the loading of the "customize" file
(add-to-list 'load-path (expand-file-name "config/" user-emacs-directory)) ;; user-emacs-directory + "config/" to put the config directory in the load-path

;; Start a server version of Emacs
(server-start)

;; Set up my personal information and my personal settings
(require 'personal-info)
(require 'personal-settings)

;;;; Load in my package list
(require 'package-config)

;;;; Load in my current theme
(require 'theme-config)

;;;; Load in Magit options
(require 'magit-config)

;;;; Load in interactive file managers for Emacs
;;(require 'neotree-config)
(require 'treemacs-config)

;;;; Major mode configuration and loading
(require 'org-mode-config) ;; org-mode configuration
(require 'markdown-config) ;; markdown configuration

;;;; TeX/LaTeX (AucTeX) options
(require 'auctex-config)
(require 'auctex-latexmk-config)
(require 'reftex-config) ;; RefTeX is part of Emacs, but it's getting its own config file
(require 'preview-latex-config)
(require 'pdf-tools-config)

;;;; Nix stuff.  For editing *.nix files (Nix and NixOS)
;; But only if the system is a GNU/Linux system, because Nix only supports those
(when (equal system-type 'gnu/linux)
  (require 'nix-config)
  )

;;;; Haskell Configs
(require 'haskell-config)

;;;; Java/Eclim Configs
(require 'eclim-config)

;;;; Multiple Cursors
(require 'multiple-cursors-config)

;;;; Gradle Build System
(require 'gradle-config)

;;;; flycheck for spell/syntax checking
(require 'flycheck-config)

;;;; Company and its associated packages
(require 'company-config)
(require 'company-auctex-config)
(require 'company-math-config)

;;;; Snippets are provided by Yasnippet
(require 'yasnippet-config)
(require 'yasnippet-snippets-config)

;;;; Project commands and management
(require 'projectile-config)

;;;; Tags and their configurations
(require 'ctags-config)
;;(require 'gtags-config)
;;(require 'bpr-config)
(require 'ggtags-config)

;;; init.el ends here
