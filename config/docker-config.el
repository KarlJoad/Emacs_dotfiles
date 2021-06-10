;;; docker-config.el --- This file sets up packages for workgin with Docker in Emacs
;;; Commentary:
;;; Code:

;; Emacs Interface to Docker
(use-package docker
  ; :ensure t
  :defer t
  :bind ("C-c d" . 'docker))

;; Emacs Interface to the Docker API
(use-package docker-api
  ; :ensure t
  :defer t)

;; For running various commands in Docker containers
(use-package docker-cli
  ; :ensure t
  :defer t)

;; Major mode for editing docker-compose files
(use-package docker-compose-mode
  ; :ensure t
  :defer t)

;; Major mode for editing Docker's Dockerfiles
(use-package dockerfile-mode
  ; :ensure t
  :defer t)

(provide 'docker-config)
;;; docker-config.el ends here
