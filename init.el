;;;; Karl's .emacs Init File
;;; Last Edited: 20190521

;;;; Make Emacs Start Full-Screen
(add-hook 'emacs-startup-hook 'toggle-frame-maximized)

;;;; Add Package Archives
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;;;; Ensure packages are always new and always loaded
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package-ensure) ; Make sure packages are loaded
(setq use-package-always-ensure t)
(use-package auto-compile
  :config (auto-compile-on-load-mode))
(use-package neotree)
(use-package magit)
(use-package auctex
  :defer t
  :ensure t)
(use-package markdown-mode)

;;; Loading in themes, to prevent the use of (custom-set-variables)
;;; This will make sure things are loaded in at the correct time
;;; Uncomment one of these lines, then change the load-theme line to the appropriate theme
;(use-package abyss-theme)
;(use-package cyberpunk-theme)
;(use-package doom-themes)
;(use-package spacemacs-theme)

;;;; Change Default custom-theme-load-path
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'tron t) ; Arguments: Themename No-confirm

;;;; Turn on Line numbering
(global-display-line-numbers-mode)

;;;; Org-mode Things (Agenda)
(global-set-key (kbd "C-c a") 'org-agenda) ; "C-c a" opens the Agenda Buffer to choose where to go
(global-set-key (kbd "C-c l") 'org-store-link) ; "C-c l" stores a hyperlink to the cursor's current position in the current org-mode document
; (global-set-key (kbd "C-c c") 'org-capture) ; "C-c c" will let me select a template and file the new information

;;;; Neotree organizer in left area
(require 'neotree)
(global-set-key [f8] 'neotree-toggle) ; On F8 press, toggle the neotree project browser
;(setq neo-theme (if (display-graphic-p) 'nerd 'ascii))
(setq neo-theme (if (display-graphic-p) 'nerd 'ascii))

;;;; Set the Theme
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files nil)
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
