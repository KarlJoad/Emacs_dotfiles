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
(defvar user-emacs-config-directory (concat user-emacs-directory "config/")
  "Variable for this user's configuration directory.")
(setq custom-file (concat user-emacs-config-directory "customize.el")) ;; File for things written by the "customize" stuff in emacs
;;(load-file custom-file) ;; Prevent the loading of the "customize" file
(add-to-list 'load-path (expand-file-name "config/" user-emacs-directory)) ;; user-emacs-directory + "config/" to put the config directory in the load-path

;; Start a server version of Emacs
;; First we ensure that the server.el(c) file is loaded with require
(require 'server)
;; Then we check if the function server-running-p is bound to a function
;;  which means that it is available for us to use.
(add-hook 'after-init-hook
          (lambda ()
            (unless (and (fboundp 'server-running-p))
              (server-start))))

;;;; Load in my package list
(require 'package-config)

;;;; Load certain packages VERY early, so that ANY packages that depend on it
;;;; are loaded correctly. This also goes for packages from within Emacs itself,
;;;; as those are typically outdated with regards to the packages pulled in by
;;;; straght.el.
;;;; Load project.el, using straight RIGHT NOW (ASAP), because if any packages
;;;; depend on it, they use `(require 'project)', then the one shipped with Emacs
;;;; is used, which is very old and causes problems everywhere.
(straight-use-package 'project)
(straight-use-package 'dom)

;; Set up: my personal info, my personal settings, and personal functions
(require 'personal-info)
(require 'personal-settings)
(require 'personal-functions)

;;;; Load in configuration to buffer management
(require 'buffer-manage-config)

;;;; Loading Org early is best for straight.el
(require 'org-mode-config) ;; org-mode configuration
;; Org-mode MUST be loaded before my email config, because mu4e has a dependency
;; on org-mode.

;;;; Load in my email settings
(require 'email-config)

;;;; Load in configuration for Emacs' IRC client, ERC
(require 'erc-config)

;;;; Load in Magit options
(require 'magit-config)

;;;; flycheck for spell/syntax checking
(require 'flycheck-config)

;;;; TeX/LaTeX (AucTeX) options
(require 'auctex-config)
(require 'auctex-latexmk-config)
(require 'reftex-config) ;; RefTeX is part of Emacs, but it's getting its own config file
(require 'BibTeX-config) ;; BibTeX is part of Emacs, but it's getting its own config file
(require 'preview-latex-config)
(require 'pdf-tools-config)

;;;; Company and its associated packages
(require 'company-config)
(require 'company-auctex-config)
(require 'company-math-config)

;;;; Snippets are provided by Yasnippet
(require 'yasnippet-config)
(require 'yasnippet-snippets-config)

;;;; Tags and their configurations
(require 'ctags-config)
;;(require 'gtags-config)
;;(require 'bpr-config)
(require 'ggtags-config)

;;;; Load in an Undo-Tree for reverting buffers
(require 'undo-tree-config)

;;;; Load in interactive file managers for Emacs
;;(require 'neotree-config)
;; (require 'treemacs-config)

;;;; For interaction with projects, we use project.el and projectile
;;;; Project commands and management
(require 'projectile-config)

;;;; LSP, for interacting with programming language servers
(require 'lsp-mode-config)

;;;; Major mode configuration and loading
(require 'markdown-config) ;; markdown configuration
(require 'Emacs-Lisp-config) ;; Emacs-List major mode configuration
(require 'java-mode-config) ;; Java major mode configuration
(require 'cc-mode-config)
(require 'web-mode-config)
(require 'sml-mode-config)
(require 'cobol-config) ;; This is probably a temporary config file.
(require 'scala-config)
(require 'rust-config)
(require 'haskell-config)
(require 'assembly-config)
(require 'arduino-config)
(require 'erlang-config)

;;;; Nix stuff.  For editing *.nix files (Nix and NixOS)
;; But only if the system is a GNU/Linux system, because Nix only supports those
(when (equal system-type 'gnu/linux)
  (require 'nix-config))

;; Only pull in direnv configuration if our current system is NixOS
(when (karljoad/is-nixos)
  (require 'direnv-config))

;;;; Docker packages and configuration
(require 'docker-config)

;;;; Multiple Cursors
(require 'multiple-cursors-config)

;;;; Gradle Build System
(require 'gradle-config)

;;;; Coloring things with rainbow-mode
(require 'rainbow-mode-config)

;;;; Load in my current theme
(require 'theme-config)

;;; init.el ends here
