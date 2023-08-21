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


;;; Emacs 29.1 added a the ability for Emacs' built-in package.el to upgrade
;;; some packages that are shipped with Emacs, such as flymake. Hopefully,
;;; straight.el picks this option up and upgrades these too.
(when (and (= emacs-major-version 29)
           (= emacs-major-version 1))
  (setq-default package-install-upgrade-built-in 't))

(provide 'package-config)
;;; package-config.el ends here
