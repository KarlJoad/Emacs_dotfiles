;;; quickshell-config.el --- Configure Emacs for quickshell configuration & work -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package qml-ts-mode
  :ensure (:host github :repo "xhcoding/qml-ts-mode"
           :branch "main"
           :depth nil)
  :defer t)

(provide 'quickshell-config)
;;; quickshell-config.el ends here
