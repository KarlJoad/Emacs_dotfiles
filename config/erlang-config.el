;;; erlang-config.el --- Packages and configuration for Erlang
;;; Commentary:
;;; Code:

(use-package erlang
  :straight t
  :defer t)

(use-package erlstack-mode
  :straight t
  :defer t)

(use-package edts
  :straight t
  :defer t)

(use-package company-erlang
  :straight t
  :defer t)

(provide 'erlang-config)
;;; erlang-config.el ends here
