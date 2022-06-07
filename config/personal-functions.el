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

(defun karljoad/is-nixos ()
  "Return 't' or 'nil', depending on if the current OS is NixOS."
  (with-temp-buffer
    (insert (shell-command-to-string "cat /etc/os-release"))
    (goto-char 0)
    (condition-case nil
	(progn
	  (when (not (equal (search-forward "ID=nixos" nil t)
			    nil))
	    t)))))

(defun karljoad/is-guix-system ()
  "Return t or nil, depending on if the current OS is Guix System.

This works because the `/run/current-system/profile' does NOT exist as a
symlink unsless the running OS is weird or is NOT a Guix System."
  (if (file-symlink-p "/run/current-system/profile")
      t
    nil))

(defun karljoad/etags-generate (dir-name)
  "Generate an etags TAGS file for C in the specified DIR-NAME.

Note that this uses `xargs' and `find', both of which are provided by GNU
findutils."
  (interactive "DDirectory: ")
  (eshell-command
      (format "find %s -type f -name \"*.[chS]\" | xargs etags -a" dir-name)))

(provide 'personal-functions)
;;; personal-functions.el ends here
