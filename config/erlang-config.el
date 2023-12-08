;;; erlang-config.el --- Packages and configuration for Erlang
;;; Commentary:
;;; Code:

;; Major mode for Erlang
(use-package erlang
  :straight t
  :defer t)

;; Minor mode to analyze stack traces
(use-package erlstack-mode
  :straight t
  :defer t)

;; Project management allowing for REST calls to EDTS backend server
(use-package edts
  :straight t
  :defer t)

(provide 'erlang-config)
;;; erlang-config.el ends here
