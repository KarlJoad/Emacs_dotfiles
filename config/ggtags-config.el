;;; ggtags-config.el --- Provides and configures ggtags
;;; Commentary:
;;; Code:

(use-package ggtags
  :ensure t
  :defer t
  :bind
  (:map global-map
	("C-x t c" . 'ggtags-create-tags)
	("C-x t u" . 'ggtags-update-tags)
	("C-x t f" . 'ggtags-find-file)
	("C-x t r" . 'ggtags-find-reference)
	("C-c t h" . 'ggtags-view-tag-history)))

(add-hook 'c-mode-common-hook
	  (lambda ()
	    (when (derived-mode-p 'c-mode
				  'c++-mode
				  'java-mode
				  'asm-mode
				  'python-mode))))

(ggtags-mode 1) ;; Turn ggtags on in every buffer

(provide 'ggtags-config)
;;; ggtags-config ends here
