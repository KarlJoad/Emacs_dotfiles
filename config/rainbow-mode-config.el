;;; rainbow-mode-config.el --- Provides and configures rainbow-mode -*- lexical-binding: t -*-
;;; Commentary:
;;
;; rainbow-mode is a package that colorizes the background of colors
;; of things that are present in a buffer.
;;
;;; Code:

(use-package rainbow-mode
  :straight t
  :defer t)

;; Enable Rainbow-mode for any buffer that runs the css-mode-hook
(add-hook 'css-mode-hook #'rainbow-mode)

(provide 'rainbow-mode-config)
;;; rainbow-mode-config ends here
