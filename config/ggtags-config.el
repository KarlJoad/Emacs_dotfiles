;;; ggtags-config.el --- Provides and configures ggtags
;;; Commentary:
;; This REQUIRES that the global package be installed from your distribution maintainer
;; This will likely install ctags as well, which is what we want.
;;; Code:

(use-package ggtags
  :straight t
  :defer t
  :bind
  (:map global-map
	("C-x t c" . 'ggtags-create-tags)
	("C-x t u" . 'ggtags-update-tags)
	("C-x t f" . 'ggtags-find-file)
	("C-x t r" . 'ggtags-find-reference)
	("C-x t d" . 'ggtags-find-definition)
	("C-x t h" . 'ggtags-view-tag-history)
	("C-x t s" . 'ggtags-find-other-symbol)))

(add-hook 'c-mode-common-hook
	  (lambda ()
	    (when (derived-mode-p 'c-mode
				  'c++-mode
				  'java-mode
				  'asm-mode
				  'python-mode))))

(ggtags-mode 1) ;; Turn ggtags on in every buffer

;; Auto reload tags when they're regenerated.
(setq tags-revert-without-query t)

(provide 'ggtags-config)
;;; ggtags-config ends here
