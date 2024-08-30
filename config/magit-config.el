;;; magit-config.el --- Provides and configures Magit -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Magit is a Git procelain that gives us a nice way to work with Git
;;
;;; Code:

;; Use more up-to-date packages for magit
(use-package seq
  :ensure t)

(use-package transient
  :ensure t
  :defer nil)

(use-package magit
  :ensure t
  :defer t
  :requires compat
  :bind
  (;; Open Magit Status (git status) for git handling
   ("C-x g" . #'magit-status)
   ;; Bring up a small menu to choose to do magit things
   ("C-x M-g" . #'magit-dispatch)
   ;; With `git blame`, we can find out the commits that changed certain lines and/or regions
   ("C-c b" . #'magit-blame)))

;; To make magit work, we need the compat package too
(use-package compat
  :ensure t
  :defer t)

;; Default directory to clone into is the Repos directory
(setq magit-clone-default-directory "~/Repos/")

;;; Keybindings in Magit Major Mode
;; git fetch = "f"
;; git pull = "F"
;; git push = "P"

(setq magit-auto-revert-mode t)

(provide 'magit-config)
;;; magit-config.el ends here
