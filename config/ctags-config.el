;;; ctags-config.el --- Provides and configures ctags
;;; Commentary:
;;
;; ExuberantCtags generates tags to jump around inside of projects
;; It does this based on symbols, i.e. functions, variables, classes, etc.
;; This software is NOT provided by Emacs, and must be installed separately.
;; For Windows: https://github.com/universal-ctags/ctags-win32/releases
;; For Linux: sudo <pkgmanager> ctags
;;
;;; Code:

(defvar
  path-to-ctags
  ""
  "Path to Universal ctags executable.")

(if (equal system-type 'windows-nt)
    (when (equal (system-name) "Karl-Desktop")
      (setq path-to-ctags "e:/ctags/ctags.exe"))
  ;; Else, I'm on GNU/Linux
  (setq path-to-ctags "/opt/local/bin/ctags"))

(defun create-tags (DIR-NAME)
  "Create a TAGS file from DIR-NAME."
  (interactive "Directory: ")
  (shell-command
   (format "%s -f TAGS -e -R %s" path-to-ctags (directory-file-name DIR-NAME)))
  )

(provide 'ctags-config)
;;; ctags-config ends here
