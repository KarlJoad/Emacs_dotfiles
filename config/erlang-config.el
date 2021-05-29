;;; erlang-config.el --- Packages and configuration for Erlang
;;; Commentary:
;;; Code:

(use-package erlang
  :ensure t
  :defer t)

(use-package erlstack-mode
  :ensure t
  :defer t)

(use-package edts
  :ensure t
  :defer t)

(use-package company-erlang
  :ensure t
  :defer t)

(provide 'erlang-config)
;;; erlang-config.el ends here
