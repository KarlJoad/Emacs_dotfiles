;;; nix-config.el --- Provides and configures packages for Nix and NixOS
;;; Commentary:
;;
;; NixOS is a GNU/Linux operating system built around the Nix package manager.
;; The Nix package manager is a functional, declarative, atomic package manager that focuses
;; on reproducability between computers.
;;
;;; Code:
;; Each of these is deferred because I won't necessarily edit Nix files on every GNU/Linux
;; computer that I use
(use-package nix-mode ;; Major mode for editing *.nix files
  :defer t)
(use-package json-mode ;; nix-mode needs json-mode for some reason
  :defer t)
(use-package nix-buffer
  :defer t)
(use-package nix-update
  :defer t)
(use-package nixos-options ;; Options for the Nixos .nix files
  :defer t)
(use-package company-nixos-options ;; Provide a company backend for nixos-options
  :defer t)

(provide 'nix-config)
;;; nix-config.el ends here
