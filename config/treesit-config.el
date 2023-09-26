;;; treesit-config.el --- This file provides configuration for Treesitter
;;; Commentary:
;;; Code:

(require 'personal-functions)

;; Fetch and use the treesit package when Emacs is built with treesitter support
(use-package treesit
  :straight (:type built-in)
  :when (treesit-available-p)
  :init
  (setq major-mode-remap-alist
        '((yaml-mode . yaml-ts-mode)
          (bash-mode . bash-ts-mode)
          (c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (c-or-c++-mode . c-or-c++-ts-mode)
          (js2-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode)
          (python-mode . python-ts-mode))))

(when (karljoad/is-guix-system)
  (add-to-list 'treesit-extra-load-path (getenv "TREE_SITTER_GRAMMAR_PATH")))

;; Bring paredit-like functionality to every programming language!
(use-package tree-edit)

(provide 'treesit-config)
;;; treesit-config.el ends here
