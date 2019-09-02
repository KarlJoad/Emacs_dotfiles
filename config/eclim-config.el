;;; eclim-config.el --- Settings for integrating Java with Emacs
;;; Commentary:
;;; Code:

(use-package eclim) ;; Pull the emacs/eclim integration package into Emacs

(require 'eclim)
(add-hook 'java-mode-hook 'eclim-mode)

(require 'eclimd)

;; Make sure Emacs Eclim knows where eclim even is

;; Configure autocomplete
(require 'company)
(use-package company-emacs-eclim)
(require 'company-emacs-eclim)
(company-emacs-eclim-setup)
(global-company-mode t)

;; Make sure the eclipse server daemon is running when needed
(setq eclimd-autostart t)

;; Display compilation error messages in the echo area
(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.2)
(help-at-pt-set-timer)

(provide 'eclim-config)
;;; eclim-config.el ends here
