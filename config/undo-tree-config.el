;;; undo-tree-config.el --- Settings for making Emacs mine
;;; Commentary:
;;; Code:

(use-package undo-tree
	:straight t
	:defer t
	:config
	(setq undo-tree-visualizer-diff t))

(global-undo-tree-mode)

(global-set-key (kbd "C-c u v") #'undo-tree-visualize)

(provide 'undo-tree-config)
;;; undo-tree-config.el ends here
