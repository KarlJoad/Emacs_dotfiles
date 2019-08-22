;;; neotree-config.el --- This file provides my settings for neotree package
;;; Commentary:
;;
;;
;;
;;; Code:

;;; Put the neotree organizer on the very left of the screen
(use-package 'neotree)
(global-set-key [f8] 'neotree-toggle) ;; On F8 press, toggle the neotree project browser
(setq neo-theme (if (display-graphic-p) 'nerd 'ascii)) ;; If in GUI, then use nerd fonts. Otherwise, in we're in terminal, then use ASCII

(provide 'personal-neotree)
;;; neotree-config.el ends here
