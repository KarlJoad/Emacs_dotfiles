;;; multiple-cursors-config.el --- Provides and configures the multiple-cursors package
;;; Commentary:
;;; Code:

(use-package multiple-cursors)

;; Place cursor on each line in selected region
(keymap-global-set "C-S-c C-S-C" 'mc/edit-lines)

;; Place cursors based on keywords in buffer
(keymap-global-set "C->" 'mc/mark-next-like-this)
(keymap-global-set "C-<" 'mc/mark-previous-like-this)
(keymap-global-set "C-c C-<" 'mc/mark-all-like-this)

(provide 'multiple-cursors-config)
;;; multiple-cursors-config ends here
