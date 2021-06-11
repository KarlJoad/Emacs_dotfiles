;;; projectile-config.el --- Provides and configures projectile for my needs
;;; Commentary:
;;
;; projectile gives us a nice way to move through and around projects.
;; projectile will automatically recognize most version-controlled directories as projects
;; It works particularly well with Git.
;; However, if you want to designate a directory as a project, make a `.projectile` file.
;;
;;; Code:

(use-package projectile
  :straight t
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
  (setq projectile-enable-caching t))

(provide 'projectile-config)
;;; projectile-config.el ends here
