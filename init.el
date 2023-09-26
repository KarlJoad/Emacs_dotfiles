;;; init.el --- Karl's .emacs Init File
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
(use-package no-littering)
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

;;;; Load in my current theme
(require 'theme-config)

;;;; Load certain packages VERY early, so that ANY packages that depend on it
;;;; are loaded correctly. This also goes for packages from within Emacs itself,
;;;; as those are typically outdated with regards to the packages pulled in by
;;;; straght.el.
;;;; Load project.el, using straight RIGHT NOW (ASAP), because if any packages
;;;; depend on it, they use `(require 'project)', then the one shipped with Emacs
;;;; is used, which is very old and causes problems everywhere.
(straight-use-package 'project)
;; (straight-use-package 'dom)

;;;; Load config to make navigating through Emacs and files easier.
(require 'navigation-config)

;;;; Load in configuration to buffer management
(require 'buffer-manage-config)

;;;; Load in an Undo-Tree for reverting buffers
(require 'undo-tree-config)

;;;; Bring in tree-sitter support for Emacs
;; (require 'tree-sitter-config)

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

;;;; flycheck for spell/syntax checking
(require 'flycheck-config)

;;;; Snippets are provided by Yasnippet
(require 'yasnippet-config)
(require 'yasnippet-snippets-config)

;;;; Company and its associated packages
;;; Company MUST be loaded very early, to make sure that all programming language
;;; stuff can assume it exists.
(require 'company-config)

;;;; TeX/LaTeX (AucTeX) options
(require 'latex-config)

(require 'pdf-tools-config)

;;;; Tags and their configurations. For now, I just use etags, so no config.
;; (require 'ctags-config)
;; (require 'gtags-config)
(require 'ggtags-config)

;;;; For interaction with projects, we use project.el and projectile
;;;; Project commands and mannagement
(require 'projectile-config)

;;;; LSP, for interacting with programming language servers
(require 'lsp-config)

;;;; Major mode configuration and loading
(require 'cc-mode-config) ;; C/C++
(require 'kconfig-config)
(require 'scheme-config)
(require 'guile-config)
(require 'common-lisp-config)
(require 'stumpwm-config)
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

;;;; Docker packages and configuration
(require 'docker-config)

;; (require 'test-config)
(require 'eshell-config)

;;; init.el ends here
