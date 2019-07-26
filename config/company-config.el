;;; company-config.el --- Provides and configures company-mode
;;; Commentary:
;;
;; Provides and configures CompleteAny-mode
;;
;;; Code:

;; Company gives us an engine for some auto-completion for things we provide a backend for
(use-package company
  :ensure t
  :defer t
  :init
  (global-company-mode)
  :config
  (setq company-idle-delay 0)
  (define-key company-active-map (kbd "<escape>") 'company-abort)
  (define-key company-active-map [tab] 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

;; Company-AUCTeX provides the backend for Company
;;(use-package company-auctex)
;;(company-auctex-init)

;; Company-math
(use-package company-math)

(provide 'company-config)
;;; company-config.el ends here
