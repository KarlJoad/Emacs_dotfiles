;;; theme-config.el --- This file provides my theming options and configuration
;;; Commentary:
;;; Code:

;;; Uncomment one of these lines, then change the load-theme line to the appropriate theme
;;(use-package abyss-theme)
;;(use-package cyberpunk-theme)
(use-package doom-themes
  :defer t
  :init (load-theme 'doom-outrun-electric t)
  :config
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)
  (doom-themes-org-config))
;; (use-package spacemacs-theme ;; Load the spacemacs themes up
;;   :defer t
;;   :init (load-theme 'spacemacs-dark t)) ;; Load the dark theme up

;;; Change Default custom-theme-load-path
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes/"))

;;; Load the theme I want
;;(load-theme 'tron t) ;; Arguments: Themename No-confirm
;; Commented out for Spacemacs theme

;;; This function will make sure that if we start Emacs with emacs --daemon,
;;; each subsequent frame with have the correct theme
(defun apply-theme-if-daemon ()
  "Apply the theme used above if Emacs is evaluated with `emacs --daemon`, ensuring each subsequent frame is themed appropriately."
  (interactive) ;; This can be calld with M-x apply-theme-if-daemon
  (load-theme 'doom-outrun-electric t)) ;; CHANGE ME WHEN YOU CHANGE THE EARLIER load-theme

(if (daemonp)
    (add-hook 'after-make-frame-functions
	      (lambda (frame)
		(with-selected-frame frame (apply-theme-if-daemon))))
  (apply-theme-if-daemon))

(provide 'theme-config)
;;; theme-config.el ends here
