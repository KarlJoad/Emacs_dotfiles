;;; test-config.el --- Settings for making Test(s) workable -*- lexical-binding: t -*-
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
;; (keymap-global-set "C-s-a" #'embark-act)

(use-package project-shells)
(global-project-shells-mode)
;; (keymap-global-set "C-c C-s" #'project-shells-keymap-prefix)
;; (project-shells-setup projectile-mode-map)

(provide 'test-config)
;;; test-config.el ends here
