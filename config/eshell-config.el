;;; eshell-config.el --- Settings for making Eshell(s) workable
;;; Commentary:
;;; Code:

(global-set-key (kbd "s-s") #'eshell)

(add-hook
 'eshell-mode-hook
 (lambda ()
   (if (or envrc-global-mode direnv-global-mode)
       (add-hook 'envrc-mode-hook (lambda () (setenv "PAGER" "")))
     (setenv "PAGER" ""))))

(provide 'eshell-config)
;;; eshell-config.el ends here
