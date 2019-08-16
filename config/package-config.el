;;; package-config-el --- Lists packages to load into Emacs
;;; Commentary:
;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;;(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))

;;; Pin some packages to specific repositories
(setq package-pinned-packages '((gtags . "marmalade")))

(when (equal system-type 'gnu/linux)
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")) ;; This is done because of Bug #34341. Should be fixed in Emacs 27
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341

(package-initialize)

;;; Ensure packages are always new and always loaded
(when (not (package-installed-p 'use-package)) ; When use-package is not installed,
  (package-refresh-contents nil)               ; refresh package repositories, synchronously
  (package-install 'use-package))              ; and install use-package
(require 'use-package-ensure) ; Make sure use-package is loaded, and ready to compile stuff
(setq use-package-always-ensure t) ; Make sure that we can always ensure packages are ready
(use-package auto-compile ; If we just downloaded a package, then compile it too
  :config (auto-compile-on-load-mode)) ; Automatically compile packages on first load

(provide 'package-config)
;;; package-config.el ends here
