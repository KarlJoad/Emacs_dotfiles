;;; markdown-config.el --- Provides and configures markdown-mode for me -*- lexical-binding: t -*-
;;; Commentary:
;;
;; This major mode provides syntax highlighting, among other things, that makes it easier
;; to write markdown files.
;;
;;; Code:

(use-package markdown-mode
  :ensure t
  :defer t
  :custom
  (markdown-fontify-code-block-natively t))

(provide 'markdown-config)
;;; markdown-config.el ends here
