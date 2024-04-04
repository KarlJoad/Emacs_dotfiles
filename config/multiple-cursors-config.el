;;; multiple-cursors-config.el --- Provides and configures the multiple-cursors package -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package multiple-cursors
  :straight t
  :bind (;; Place cursor on each line in selected region
         ("C-S-c C-S-C" . #'mc/edit-lines)
         ;; Place cursors based on keywords in buffer
         ("C->" . #'mc/mark-next-like-this)
         ("C-<" . #'mc/mark-previous-like-this)
         ("C-c C-<" . #'mc/mark-all-like-this)))

(provide 'multiple-cursors-config)
;;; multiple-cursors-config ends here
