;;; rainbow-mode-config.el --- Provides and configures rainbow-mode -*- lexical-binding: t -*-
;;; Commentary:
;;
;; rainbow-mode is a package that colorizes the background of colors
;; of things that are present in a buffer.
;;
;;; Code:

(use-package rainbow-mode
  :ensure t
  :defer t
  ;; Enable Rainbow-mode for any buffer that runs the css-mode-hook
  :hook ((css-mode . rainbow-mode)))

(provide 'rainbow-mode-config)
;;; rainbow-mode-config ends here
