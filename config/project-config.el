;;; project-config.el --- Provides and configures project.el for my needs -*- lexical-binding: t -*-
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

(use-package xref
  :ensure t
  :defer nil)

;; We fetch the most recent version of project.el rather than use the one built
;; into Emacs, for no particular reason.
(use-package project
  :ensure t
  :defer nil
  :requires (xref)
  :after (magit)
  :custom
  ;; Add magit-status as a possible project.el keybinding
  (add-to-list project-switch-commands (list #'magit-status "Magit"))
  (define-key project-prefix-map "m" #'magit-status))

(defun project-whitespace-cleanup-project-files ()
  "Run `whitespace-cleanup' on all project files.
NOTE: This will also close your currently open buffer if you are visiting a file
in the project."
  (interactive)
  (require 'project)
  (dolist (file-path (project-files (project-current)))
    (let* ((buffer (find-file-noselect file-path)))
      (when buffer
        (with-current-buffer buffer
          (whitespace-cleanup)
          (save-buffer)
          (kill-buffer))))))

(provide 'project-config)
;;; project-config.el ends here
