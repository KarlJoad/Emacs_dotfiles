;;; web-mode-config.el --- Provides and configures rainbow-mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package web-mode
  :ensure t
  :defer t
  :hook (web-mode . rainbow-mode))

(provide 'web-mode-config)
;;; web-mode-config ends here
