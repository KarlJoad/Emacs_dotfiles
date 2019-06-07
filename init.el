;;;; Karl's .emacs Init File
;;; Last Edited: 20190521

;;;; Make Emacs Start Full-Screen
(add-hook 'emacs-startup-hook 'toggle-frame-maximized)

;;;; Tell Emacs where to look for my other config files
(setq user-emacs-directory (expand-file-name "~/.emacs.d/")) ; Directory where Emacs Files are located
(setq user-emacs-config-directory (concat user-emacs-directory "config/"))
(add-to-list 'load-path (concat user-emacs-directory "config/")) ; user-emacs-directory + "config/" to put the config directory in the load-path

;;;; Make sure Emacs loads up newer config files, even if they aren't compiled
(setq load-prefer-newer t)

;;;; Set up my personal information and my personal settings
(load "personal-info")
(load "personal-settings")

;;;; Add Package Archives
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;;;; Ensure packages are always new and always loaded
(when (not (package-installed-p 'use-package)) ; When use-package is not installed,
  (package-refresh-contents)                   ; refresh package repositories,
  (package-install 'use-package))              ; and install use-package
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
(use-package auctex-latexmk)
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

;;;; Org-mode Things (Agenda)
(global-set-key (kbd "C-c a") 'org-agenda) ; "C-c a" opens the Agenda Buffer to choose where to go
(global-set-key (kbd "C-c l") 'org-store-link) ; "C-c l" stores a hyperlink to the cursor's current position in the current org-mode document
; (global-set-key (kbd "C-c c") 'org-capture) ; "C-c c" will let me select a template and file the new information

;;;; Load in Neotree options
(load "neotree-config")

;;;; Load in Magit options
(load "magit-config")

;;;; AucTeX options
(load "auctex-config")

;;; preview-latex Options
;(load "preview-latex-config.el" nil t t) ; noerror-nil, nomessage-t, nosuffix-t

;;;; Lastly, Change Directory to where I want to work
(cd "c:/users/karl/documents/git")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (use-package spacemacs-theme neotree markdown-mode magit auto-compile auctex-latexmk all-the-icons))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
