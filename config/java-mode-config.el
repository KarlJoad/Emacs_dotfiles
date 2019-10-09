;;; java-mode-config.el --- Settings for making Emacs work in Java-mode
;;; Commentary:
;;; Code:

(add-hook 'java-mode-hook (lambda ()
			    (setq c-basic-offset 4
				 tab-width 4
				 indent-tabs-mode nil)))
;; By setting indent-tabs-mode to nil, when I press <TAB>, I insert 4 spaces instead

(provide 'java-mode-config)
;;; java-mode-config.el ends here
