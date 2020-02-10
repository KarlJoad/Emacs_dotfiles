;;; undo-tree-config.el --- Settings for making Emacs mine
;;; Commentary:
;;; Code:

(use-package undo-tree
	     :ensure t
	     :defer t
		 :config
		 (global-undo-tree-mode t)
		 (setq undo-tree-visualizer-diff t))

(provide 'undo-tree-config)
;;; undo-tree-config.el ends here
