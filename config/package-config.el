;;; package-config-el --- Lists packages to load into Emacs
;;; Commentary:
;;; Code:

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq straight-use-package-by-default t) ;; If I use use-package, use straight instead

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;;; Ensure packages are always new and always loaded
(straight-use-package 'use-package)
(when (not (package-installed-p 'use-package)) ;; When use-package is not installed,
  (package-refresh-contents nil)               ;; refresh package repositories, synchronously
  (package-install 'use-package))              ;; and install use-package

(provide 'package-config)
;;; package-config.el ends here
