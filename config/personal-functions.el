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

(defvar karljoad--in-presentation-mode-p 'nil
  "Is Emacs currently in \"presentation mode\"?")

;; Modified from Adrien Brochard's configuration.org
(defun karljoad/toggle-presentation ()
  "If NEW-FONT-HEIGHT provided, toggle presentation features, like font increase."
  (interactive)
  (require 'personal-settings)
  (let ((presentation-fontsize 200))
    (if karljoad--in-presentation-mode-p
        (set-face-attribute 'default nil :height karljoad--default-font-height)
      (set-face-attribute 'default nil :height presentation-fontsize))
    (setq karljoad--in-presentation-mode-p (not karljoad--in-presentation-mode-p))))

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

(defun karljoad/remove-file-whitespace ()
  "Remove trailing whitespace from files.

Opens and removes all trailing whitespace from the list of files selected in a
Dired buffer. Trailing whitespace includes empty newlines at the end of the
file."
  (interactive)
  (require 'dired)
  (mapc (lambda (file-name)
          (with-temp-file file-name
            (insert-file-contents-literally file-name)
            (delete-trailing-whitespace (point-min) nil)))
        (dired-get-marked-files nil)))

(defun karljoad/gc-events ()
  "Print message about GC statistics."
  (interactive)
  (message "%d GC Events\n%0.2f Seconds spent GC-ing" gcs-done gc-elapsed))

(provide 'personal-functions)
;;; personal-functions.el ends here
