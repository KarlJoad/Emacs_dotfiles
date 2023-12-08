;;; envrc-config.el --- Settings for making Envrc(s) workable
;;; Commentary:
;;; Code:

(use-package envrc
  :straight t
  :defer t
  :config
  (envrc-global-mode)
  :bind (("C-c e" . #'envrc-command-map)))

(provide 'envrc-config)
;;; envrc-config.el ends here
