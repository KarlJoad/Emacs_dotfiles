;;; test-config.el --- Settings for making Test(s) workable
;;; Commentary:
;;; Code:

;;; Several different completion frameworks
;; As of now, vertico is my preferred one

(use-package selectrum)

(use-package orderless)

;; A few more useful configurations...

;; (use-package consult)
;; (setq completion-in-region-function #'consult-completion-in-region)

;; (use-package embark)
;; (use-package embark-consult)
;; (global-set-key (kbd "C-s-a") #'embark-act)

(use-package project-shells)
(global-project-shells-mode)
;; (global-set-key (kbd "C-c C-s") #'project-shells-keymap-prefix)
;; (project-shells-setup projectile-mode-map)

(provide 'test-config)
;;; test-config.el ends here
