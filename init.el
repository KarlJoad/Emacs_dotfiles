;;;; Karl's .emacs Init File
;;; Last Edited: 20190521

;;;; Make Emacs Start Full-Screen
(add-hook 'emacs-startup-hook 'toggle-frame-maximized)

;;;; Tell Emacs where to look for my other config files
(setq user-emacs-directory (expand-file-name "~/.emacs.d/")) ; Directory where Emacs Files are located
(setq user-emacs-config-directory (concat user-emacs-directory "config/"))
(add-to-list 'load-path (concat user-emacs-directory "config/")) ; user-emacs-directory + "config/" to put the config directory in the load-path

;;;; Start a server version of Emacs
(server-start)

;;;; Make sure Emacs loads up newer config files, even if they aren't compiled
(setq load-prefer-newer t)

;;;; Set up my personal information and my personal settings
(load "personal-info")
(load "personal-settings")

;;;; Add Package Archives
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;;;; Load in my package list
(load "package-config")

;;;; Load in my current theme
(load "theme-config")

;;;; Load in Org-mode configuration
(load "org-mode-config")

;;;; Load in Neotree options
(load "neotree-config")

;;;; Load in Magit options
(load "magit-config")

;;;; AucTeX options
(load "auctex-config")

;;; preview-latex Options
; (load "preview-latex-config"  nil t t) ; noerror-nil, nomessage-t, nosuffix-t

;;;; Lastly, Change Directory to where I want to work
(cd "c:/users/karl/documents/git")
