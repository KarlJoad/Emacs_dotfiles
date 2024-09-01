;;; magit-config.el --- Provides and configures Magit -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Magit is a Git procelain that gives us a nice way to work with Git
;;
;;; Code:

;; Use more up-to-date packages for magit
(use-package seq
  :ensure t)

;; To make magit work, we need the compat package too
(use-package compat
  :ensure t
  :defer t)

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
   ("C-c b" . #'magit-blame))
  :custom
  (magit-no-confirm '(stage-all-changes unstage-all-changes))
  (magit-clone-default-directory "~/Repos/")
  (magit-auto-revert-mode t))

;; Display TODO/FIXME/other tagged items in the repository in the magit-status
;; buffer.
(use-package magit-todos
  :ensure (:host github :repo "KarlJoad/magit-todos"
           :branch "improve-git-grep-pcre2-scanner-test")
  :demand t ; Use :demand, because we still autoload magit
  :after magit
  :custom
  (magit-todos-keywords-list (mapcar #'car hl-todo-keyword-faces))
  (magit-todos-auto-group-items 50)
  (magit-todos-exclude-globs '(".git/"))
  :config
  (magit-todos-mode))

(provide 'magit-config)
;;; magit-config.el ends here
