;;; envrc-config.el --- Settings for making Envrc(s) workable -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package envrc
  :ensure t
  :config
  (envrc-global-mode)
  :bind (("C-c e" . #'envrc-command-map)))

(provide 'envrc-config)
;;; envrc-config.el ends here
