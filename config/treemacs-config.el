;;; treemacs-config.el --- Provides and configures treemacs
;;; Commentary:
;;
;; Treemacs provides a way to have a file hierarchy viewer in the Emacs window.
;; This is an interactive viewer, that updates as I do things in the project/directory.
;;; Code:

(use-package treemacs
  :ensure t
  :defer t
  :bind
  (:map global-map
	("M-0" . treemacs-select-window)
	("C-x t 1" . treemacs-delete-other-windows)
	("C-x t t" . treemacs)
	("C-x t B" . treemacs-bookmark)
	("C-x t C-t" . treemacs-find-file)
	("C-x t M-t" . treemacs-find-tag)))

;; Treemacs-magit fills in the gaps that treemacs can't handle for git/magit
(use-package treemacs-magit
  :ensure t
  :defer t)

;; Allows me to use treemacs icons in dired buffers
(use-package treemacs-icons-dired
  :ensure t
  :defer t)

(provide 'treemacs-config)
;;; treemacs-config.el ends here
