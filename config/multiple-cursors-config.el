;;; multiple-cursors-config.el --- Provides and configures the multiple-cursors package
;;; Commentary:
;;; Code:

(use-package multiple-cursors)

;; Place cursor on each line in selected region
(global-set-key (kbd "C-S-c C-S-C") 'mc/edit-lines)

;; Place cursors based on keywords in buffer
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(provide 'multiple-cursors-config)
;;; multiple-cursors-config ends here
