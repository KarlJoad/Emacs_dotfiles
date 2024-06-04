;;; racket-config.el --- Configure Racket for Emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'magit)
(require 'lispy-config)
(require 'yasnippet-config)

(use-package racket-mode
  :ensure t
  :hook ((racket-mode . racket-xp-mode)
         (racket-mode . racket-unicode-input-method-enable)
         (racket-repl-mode . racket-unicode-input-method-enable)
         (racket-mode . (lambda () (set-input-method 'TeX))))
  :bind (:map racket-mode-map
         ("C-c C-z" . racket-run-and-switch-to-repl)))

(provide 'racket-config)
;;; racket-config.el ends here
