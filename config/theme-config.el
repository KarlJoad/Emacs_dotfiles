;;; theme-config.el --- This file provides my theming options and configuration
;;; Commentary:
;;; Code:

(use-package modus-themes ;; BOTH light (vivendi) and dark (operandi)
	:straight t
	:defer t
  :bind ("C-c T" . #'modus-themes-toggle)
	:custom
	(modus-vivendi-theme-section-headings t)
	(modus-vivendi-theme-slanted-constructs t)
	(modus-vivendi-theme-bold-constructs t)
	(modus-vivendi-theme-proportional-fonts nil)
  (modus-operandi-theme-slanted-constructs t)
  (modus-operandi-theme-bold-constructs t)
  (modus-operandi-theme-proportional-fonts nil))

(defconst karljoad/default-start-theme 'modus-vivendi
  "The theme to load by default.")

(load-theme karljoad/default-start-theme t)

(provide 'theme-config)
;;; theme-config.el ends here
