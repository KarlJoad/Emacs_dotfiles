;;; magit-config.el --- ;; Provides and configures Magit
;;; Commentary:
;;
;; Magit is a Git procelain that gives us a nice way to work with Git
;;
;;; Code:

(use-package magit)

;; Open Magit Status for git handling
(global-set-key (kbd "C-x g") 'magit-status) ;; C-x g will bring up magit status (git status)
;; Bring up a small menu to choose to do magit things
(global-set-key (kbd "C-x M-g") 'magit-dispatch)

;; With `git blame`, we can find out the commits that changed certain lines and/or regions
(global-set-key (kbd "C-c b") 'magit-blame)

;; Default directory to clone into is the Repos directory
(setq magit-clone-default-directory "~/Repos/")

;;; Keybindings in Magit Major Mode
;; git fetch = "f"
;; git pull = "F"
;; git push = "P"

(setq magit-auto-revert-mode t)

(provide 'magit-config)
;;; magit-config.el ends here
