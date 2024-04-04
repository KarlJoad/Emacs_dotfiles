;;; erlang-config.el --- Packages and configuration for Erlang -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Major mode for Erlang
(use-package erlang
  :ensure t
  :defer t)

;; Minor mode to analyze stack traces
(use-package erlstack-mode
  :ensure t
  :defer t)

;; Project management allowing for REST calls to EDTS backend server
(use-package edts
  :ensure t
  :defer t)

(provide 'erlang-config)
;;; erlang-config.el ends here
