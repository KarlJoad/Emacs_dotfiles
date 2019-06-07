;;;; Provides my personal configurations for Magit
(provide 'personal-magit)

;;; Open Magit Status for git handling
(global-set-key (kbd "C-x g") 'magit-status) ; C-x g will bring up magit status (git status)

;;; git pull with now use p to pull
(local-set-key (kbd "p") 'magit-pull) ; Using p in the magit major mode will pull that repo
