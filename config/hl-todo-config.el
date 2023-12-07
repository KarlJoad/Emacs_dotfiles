;;; ht-todo-config.el --- Configure ht-todo package
;;; Commentary:
;;
;; hl-todo is used to highlight keywords inside of buffers.
;; By default, it highlights TODO, FIXME, and NOTE.
;; You can also choose what words should be recognized and what color they should
;; be highlighted with my modifying the hl-todo-keyword-faces variable.
;;
;;; Code:

(use-package hl-todo
  :straight t
  :ensure t
  :defer t)

(global-hl-todo-mode)

(define-key hl-todo-mode-map (kbd "C-c C-t p") 'hl-todo-previous)
(define-key hl-todo-mode-map (kbd "C-c C-t n") 'hl-todo-next)

;; (setq hl-todo-keyword-faces
;;       '(("TODO"   . "#FF0000")
;;         ("FIXME"  . "#FF0000")
;;         ("DEBUG"  . "#A020F0")
;;         ("GOTCHA" . "#FF4500")
;;         ("STUB"   . "#1E90FF")))

(provide 'hl-todo-config)
;;; hl-todo-config.el ends here
