;;; project-config.el --- Provides and configures project.el for my needs
;;; Commentary:
;;
;; project.el gives us a nice way to move through and around projects.
;; project.el will automatically recognize most version-controlled directories
;; as projects.
;; It works particularly well with Git.
;;
;; See Emacs's internal documentation (emacs) Projects for further details about
;; project.el.
;;
;;; Code:

(require 'magit-config)

(use-package project
  :straight (:type built-in)
  :ensure t
  :defer t
  :after (magit)
  :custom
  (add-to-list project-switch-commands (list #'magit-status "Magit")))

(defun project-whitespace-cleanup-project-files ()
  "Run `whitespace-cleanup' on all project files."
  (interactive)
  (dolist (file (project-current-project-files))
    (let* ((path (concat (project-project-root) file))
           (buffer (find-file-noselect path)))
      (when buffer
        (with-current-buffer buffer
          (whitespace-cleanup)
          (save-buffer)
          (kill-buffer))))))

(provide 'project-config)
;;; project-config.el ends here
