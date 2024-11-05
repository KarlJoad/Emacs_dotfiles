;;; container-config.el --- Working with Docker/Podman Containers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Emacs Interface to Docker/Podman
(use-package docker
  :ensure t
  :defer t
  :bind ("C-c d" . 'docker)
  :config
  (defvar docker-executable 'podman
    "The executable to be used with docker-mode.
Acceptable values are `'docker' and `'podman'.")
  (cond ((eq docker-executable 'docker)
         (progn (setq docker-command "docker")
                (setq docker-compose-command "docker-compose")))
        ((eq docker-executable 'podman)
         (progn (setq docker-command "podman")
                (setq docker-compose-command "podman-compose")))))

;; Major mode for editing Containerfiles/Dockerfiles
(use-package dockerfile-mode
  :ensure t
  :defer t
  :after (docker)
  :mode ("\\Containerfile\\'" "\\Dockerfile\\'")
  :config
  (setq dockerfile-mode-command
        (cond ((eq docker-executable 'docker) "docker")
              ((eq docker-executable 'podman) "podman")
              (t "podman"))))

(provide 'container-config)
;;; container-config.el ends here
