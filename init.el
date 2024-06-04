;;; init.el --- Karl's .emacs Init File -*- lexical-binding: t -*-
					;-*-Emacs-Lisp-*-
;;; Commentary:
;;
;; Karl's Emacs init file, in all of it's weird glory
;;
;;; Code:

;; Tell Emacs where to look for my other config files
(defvar user-emacs-config-directory (concat user-emacs-directory "config/")
  "Variable for this user's configuration directory.")

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

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))
;; Use no-littering to automatically set common paths to the new user-emacs-directory
(use-package no-littering
  :ensure t
  :defer nil)

;; Keep customization settings in a temporary file (thanks Ambrevar & Daviwil!)
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

;; Set up: my personal info, my personal settings, and personal functions
(require 'personal-info)
(require 'personal-settings)
(require 'personal-functions)

;; Configure dired
(require 'dired-config)

;;;; Load in my current theme
(require 'theme-config)

;; Load certain packages VERY early, so that ANY packages that depend on it are
;; loaded correctly. Load project.el, using straight RIGHT NOW (ASAP), because
;; if any packages depend on it, they use `(require 'project)', then the one
;; shipped with Emacs is used, which is very old and causes problems everywhere.
;; For interaction with projects, we use project.el
;; Project commands and mannagement
(require 'project-config)

;;;; Load config to make navigating through Emacs and files easier.
(require 'navigation-config)

;; Configure Emacs' completion options, ordering, and display
(require 'completion-config)

;;;; Load in configuration to buffer management
(require 'buffer-manage-config)

;;;; Load in an Undo-Tree for reverting buffers
(require 'undo-tree-config)

;;;; Set up a proper terminal emulator in Emacs.
;;; term-mode and ansi-term are alright, but vterm is better.
(when (karljoad/is-guix-system)
  (require 'vterm-config))

;;;; Loading Org early is best for straight.el
(require 'org-mode-config) ;; org-mode configuration
;; Org-mode MUST be loaded before my email config, because mu4e has a dependency
;; on org-mode.

;;;; Load in my email settings
(require 'email-config)

;;;; Load Elfeed config, to read RSS feeds
(require 'elfeed-config)

;;;; Load in configuration for Emacs' IRC client, ERC
(require 'erc-config)

;;;; Color color codes in-buffer
(require 'rainbow-mode-config)

;;;; Multiple Cursors
(require 'multiple-cursors-config)

;;;; Load in Magit options
(require 'magit-config)
(require 'ediff-config)

;;;; flycheck for spell/syntax checking
(require 'flycheck-config)

;;;; Snippets are provided by Yasnippet
(require 'yasnippet-config)

;;;; Configuration for TeX and all of TeX's variations/extensions
(require 'tex-config)

;;;; LSP, for interacting with programming language servers
(require 'lsp-config)

;;;; Treesit for semantic highlighting & editing of source code
;;;; I choose to only use the version of tree-sitter support built INTO Emacs,
;;;; which requires Emacs to be >29 AND be configured with:
;;;; ./configure --with-tree-sitter.
;;;; We first check the version of Emacs before going on and potentially loading
;;;; treesit & its changing the major-mode-remap-alist.
(when (>= emacs-major-version 29)
  (require 'treesit-config))

;;;; Major mode configuration and loading
(require 'prog-mode-config)
(require 'cc-mode-config) ;; C/C++
(require 'kconfig-config)
(require 'scheme-config)
(require 'guile-config)
(require 'common-lisp-config)
(require 'racket-config)
(require 'markdown-config)
(require 'rust-config)
(require 'web-mode-config)
(require 'scala-config)
(require 'haskell-config)
(require 'assembly-config)
(require 'erlang-config)
(require 'typescript-config)

(when (equal system-type 'gnu/linux)
  (require 'nix-config)
  (require 'guix-config))

;; Only pull in direnv configuration if our current system is NixOS or Guix System
(when (or (karljoad/is-nixos) (karljoad/is-guix-system))
  (require 'functional-packaging-config)
  (require 'envrc-config))

;; Configure various shells and their behavior
(require 'shell-config)
(require 'eshell-config)

;; (require 'test-config)

;; If Emacs has been idle for 15+ seconds, perform a GC run.
;; From https://akrl.sdf.org/#orgc15a10d
(run-with-idle-timer 15 t #'garbage-collect)

;;; init.el ends here
