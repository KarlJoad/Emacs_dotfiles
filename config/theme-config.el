;;; theme-config.el --- This file provides my theming options and configuration
;;; Commentary:
;;; Code:

(if (display-graphic-p) ;; If using GUI, then use GUI theme
;;; Uncomment one of these lines, then change the load-theme line to the appropriate theme
    (progn
      ;; (use-package abyss-theme)
      ;; (use-package cyberpunk-theme)
      ;; (use-package spacemacs-theme
      ;;   :defer t
      ;;   :init (load-theme 'spacemacs-dark t)) ;; Load the dark theme
      (use-package modus-vivendi-theme ;; The dark theme
		:defer t
		:straight t
		:config
		(setq modus-vivendi-theme-section-headings t)
		(setq modus-vivendi-theme-slanted-constructs t)
		(setq modus-vivendi-theme-bold-constructs t)
		(setq modus-vivendi-theme-proportional-fonts nil))
      ;; (setq modus-vivendi-theme-headings
      ;; 	    ''((1 . highlight)
      ;; 	       (2 . line)
      ;; 	       (t . rainbow-line-no-bold)))
      ;; (use-package modus-operandi-theme ;; The light theme
      ;; 	:defer t
      ;; 	:straight t
      ;; 	:config
      ;; 	(setq modus-operandi-theme-slanted-constructs t)
      ;; 	(setq modus-operandi-theme-bold-constructs t)
      ;; 	(setq modus-operandi-theme-proportional-fonts nil)))
      (use-package doom-themes
		;;   :defer t
		;;   :init ; (load-theme 'doom-outrun-electric t)
		:init (load-theme 'doom-one t)
		:config
		(setq doom-themes-enable-bold t)
		(setq doom-themes-enable-italic t)
		(setq doom-themes-treemacs-theme "doom-colors")
		(doom-themes-treemacs-config)
		(doom-themes-org-config)))
  (use-package monokai-theme))

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
(global-set-key (kbd "C-c T") 'karljoad/modus-themes-toggle)

(if (daemonp)
    (add-hook 'after-make-frame-functions
	      (lambda (frame)
		(with-selected-frame frame (apply-theme-if-daemon))))
  (apply-theme-if-daemon))

(provide 'theme-config)
;;; theme-config.el ends here
