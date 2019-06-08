;;;; Provides my personal configurations for Magit
(provide 'personal-magit)

;;; Open Magit Status for git handling
(global-set-key (kbd "C-x g") 'magit-status) ; C-x g will bring up magit status (git status)

;;; Bring up a small menu to choose to do magit things
(global-set-key (kbd "C-x M-g") 'magit-dispatch)

;;; Keybindings in Magit Major Mode
;; git fetch = "f"
;; git pull = "F"
;; git push = "P"
