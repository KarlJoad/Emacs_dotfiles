;;; theme-config.el --- This file provides my theming options and configuration
;;; Commentary:
;;; Code:

(use-package modus-themes ;; BOTH light (vivendi) and dark (operandi)
	:defer t
	:straight t
	:config
	(setq modus-vivendi-theme-section-headings t)
	(setq modus-vivendi-theme-slanted-constructs t)
	(setq modus-vivendi-theme-bold-constructs t)
	(setq modus-vivendi-theme-proportional-fonts nil)
  (setq modus-operandi-theme-slanted-constructs t)
  (setq modus-operandi-theme-bold-constructs t)
  (setq modus-operandi-theme-proportional-fonts nil))

(defvar current-theme 'modus-vivendi "The currently running theme.")

;;; This function will make sure that if we start Emacs with emacs --daemon,
;;; each subsequent frame with have the correct theme
(defun apply-theme-if-daemon ()
  "Apply the theme used above if Emacs is evaluated with `emacs --daemon`, ensuring each subsequent frame is themed appropriately."
  (interactive) ;; This can be calld with M-x apply-theme-if-daemon
  (load-theme current-theme t)) ;; CHANGE ME WHEN YOU CHANGE THE EARLIER load-theme

(load-theme current-theme t)

(global-set-key (kbd "C-c T") 'modus-themes-toggle)

(provide 'theme-config)
;;; theme-config.el ends here
