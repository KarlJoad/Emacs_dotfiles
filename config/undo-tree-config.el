;;; undo-tree-config.el --- Settings for making Emacs mine -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package undo-tree
	:ensure t
	:defer t
  :bind (("C-c u v" . #'undo-tree-visualize))
	:config
  (global-undo-tree-mode)
	(setq undo-tree-visualizer-diff t)
  (setq-default undo-tree-history-directory-alist
                `(("." . ,(concat no-littering-var-directory "undo-tree-hist")))))

(provide 'undo-tree-config)
;;; undo-tree-config.el ends here
