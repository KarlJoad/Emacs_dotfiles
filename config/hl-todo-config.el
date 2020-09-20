;;; ht-todo-config.el --- Configure ht-todo package
;;; Commentary:
;;; Code:

(use-package hl-todo)

(global-hl-todo-mode)

(define-key hl-todo-mode-map (kbd "C-c C-t p") 'hl-todo-previous)
(define-key hl-todo-mode-map (kbd "C-c C-t n") 'hl-todo-next)

(provide 'ht-todo-config)
;;; hl-todo-config.el ends here
