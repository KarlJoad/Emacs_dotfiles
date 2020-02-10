;;; personal-functions.el --- This file provides my exported personal functions
;;; Commentary:
;;
;; I have a file for personal information and personal settings, so it makes
;; sense to have another file whose only job is to define my personal functions.
;; These are functions that will be prefaced with karljoad/function-name.
;; These will typically only be callable through the M-x karljoad/function-name
;; route, simplifying my life a little bit.
;;
;;; Code:

;; Shamelessly stolen from Adrien Brochard's configuration.org
(defun karljoad/toggle-presentation ()
  "Toggle presentation features, like font increase."
  (interactive)
  (let ((regular-fontsize 98)
        (presentation-fontsize 200))
    (if (equal (face-attribute 'default :height) regular-fontsize) ;; If the current face-attribute's height is the regular
		;; Then switch to the presentation size
        (set-face-attribute 'default nil :height presentation-fontsize)
	  ;; Otherwise, switch back to the regular size
      (set-face-attribute 'default nil :height regular-fontsize))))


(provide 'personal-functions)
;;; personal-functions.el ends here
