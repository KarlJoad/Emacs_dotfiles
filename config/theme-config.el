;;; theme-config.el --- This file provides my theming options and configuration
;;; Commentary:
;;; Code:

(use-package abyss-theme)
(use-package cyberpunk-theme)
(use-package spacemacs-theme
  :defer t)
  ;; :init (load-theme 'spacemacs-dark t)) ;; Load the dark theme
(use-package modus-themes ;; BOTH light (vivendi) and dark (operandi)
	:defer t
	:straight t
  :init (modus-themes-load-themes)
	:config
	(setq modus-vivendi-theme-section-headings t)
	(setq modus-vivendi-theme-slanted-constructs t)
	(setq modus-vivendi-theme-bold-constructs t)
	(setq modus-vivendi-theme-proportional-fonts nil)
  (setq modus-operandi-theme-slanted-constructs t)
  (setq modus-operandi-theme-bold-constructs t)
  (setq modus-operandi-theme-proportional-fonts nil))
(use-package doom-themes
	:defer t
	;; :init ; (load-theme 'doom-outrun-electric t)
	;; :init (load-theme 'doom-one t)
	:config
	(setq doom-themes-enable-bold t)
	(setq doom-themes-enable-italic t)
	(setq doom-themes-treemacs-theme "doom-colors")
	(doom-themes-treemacs-config)
	(doom-themes-org-config))
(use-package monokai-theme)

;;; Change Default custom-theme-load-path
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes/"))

;;; Load the theme I want
;;(load-theme 'tron t) ;; Arguments: Themename No-confirm
;; Commented out for Spacemacs theme

(defvar current-theme 'modus-vivendi "The currently running theme.")

;;; This function will make sure that if we start Emacs with emacs --daemon,
;;; each subsequent frame with have the correct theme
(defun apply-theme-if-daemon ()
  "Apply the theme used above if Emacs is evaluated with `emacs --daemon`, ensuring each subsequent frame is themed appropriately."
  (interactive) ;; This can be calld with M-x apply-theme-if-daemon
  (load-theme current-theme t)) ;; CHANGE ME WHEN YOU CHANGE THE EARLIER load-theme

;; Originally Stolen from protesilaos' .emacs config, slightly modified by me
;; https://gitlab.com/protesilaos/dotemacs/-/blob/master/emacs-init.org
(defun karljoad/modus-themes-toggle ()
  "Simplistic toggle for Modus Themes.
Check if `modus-operandi' is active and if so switch to `modus-vivendi'.
The inverse applies when Vivendi is in use."
  (interactive)
  (if (eq (car custom-enabled-themes) 'modus-operandi)
	    (setq current-theme 'modus-vivendi)
    (setq current-theme 'modus-operandi))
  (load-theme current-theme t))

(global-set-key (kbd "C-c T") 'modus-themes-toggle)

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (defun karljoad/daemon-theme-init (frame)
                (with-selected-frame frame
                  (load-theme current-theme t))
                (remove-hook 'after-make-frame-functions
                             #'karljoad/theme-init-daemon)
                (fmakunbound 'karljoad/theme-init-daemon)))
  (load-theme current-theme t))

(provide 'theme-config)
;;; theme-config.el ends here
