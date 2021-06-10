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

;; Set up: my personal info, my personal settings, and personal functions
(require 'personal-info)
(require 'personal-settings)
(require 'personal-functions)

;;; init.el ends here
