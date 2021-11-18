;;; envrc-config.el --- Settings for making Envrc(s) workable
;;; Commentary:
;;; Code:

(use-package envrc
  :defer t
  :straight t)

(envrc-global-mode)

(with-eval-after-load 'envrc
  (define-key envrc-mode-map (kbd "C-c e") #'envrc-command-map))

(provide 'envrc-config)
;;; envrc-config.el ends here
