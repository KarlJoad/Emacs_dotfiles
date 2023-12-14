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
(defconst karljoad--default-font-height (face-attribute 'default :height)
  "The \"height\" of the default face when Emacs starts.")

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

(defun karljoad/this-system-this-linux-distro? (distro)
  "Return t or nil, depending on if the current OS is DISTRO."
  (interactive "MID of the distro (as found in /etc/os-release): ")
  (with-temp-buffer
    (insert (shell-command-to-string "cat /etc/os-release"))
    (goto-char 0)
    (condition-case nil
	      (progn
	        (when (not (equal (search-forward
                             (concat "ID=" distro) nil t)
			                      nil))
	          t)))))

(defun karljoad/is-nixos ()
  "Return t or nil, depending on if the current OS is NixOS."
  (karljoad/this-system-this-linux-distro? "nixos"))

(defun karljoad/is-guix-system ()
  "Return t or nil, depending on if the current OS is Guix System."
  (karljoad/this-system-this-linux-distro? "guix"))

(defun karljoad/etags-generate (dir-name)
  "Generate an etags TAGS file for C in the specified DIR-NAME.

Note that this uses `xargs' and `find', both of which are provided by GNU
findutils."
  (interactive "DDirectory: ")
  (eshell-command
      (format "find %s -type f -name \"*.[chS]\" | xargs etags -a" dir-name)))

(defun karljoad/reload-dir-locals-current-buffer ()
  "Reload `.dir-locals.el' for the current buffer.

This function is from https://emacs.stackexchange.com/a/13096"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defun dos2unix (buffer)
  "Convert BUFFER from DOS line-endings to UNIX line-endings.
This function simply replaces all instances of \r ( in Emacs character type)
with the empty character."
  (interactive "*b") ;; Name of existing buffer that is RW
  (save-excursion
    (goto-char (point-min))
    (while (search-forward (string ?\C-m) nil t)
      (replace-match (string ?\C-j) nil t))
    (set-buffer-file-coding-system 'unix 't)))

(require 'dired)
(defun karljoad/remove-file-whitespace ()
  "Remove trailing whitespace from files.

Opens and removes all trailing whitespace from the list of files selected in a
Dired buffer. Trailing whitespace includes empty newlines at the end of the
file."
  (interactive)
  (mapc (lambda (file-name)
          (with-temp-file file-name
            (insert-file-contents-literally file-name)
            (delete-trailing-whitespace (point-min) nil)))
        (dired-get-marked-files nil)))

(provide 'personal-functions)
;;; personal-functions.el ends here
