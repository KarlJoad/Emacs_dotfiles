;;; web-mode-config.el --- Provides and configures rainbow-mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package web-mode
  :straight t
  :defer t)

(add-hook 'web-mode-hook
		  (lambda ()
			(rainbow-mode)))

(provide 'web-mode-config)
;;; web-mode-config ends here
