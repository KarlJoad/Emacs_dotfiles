;;; racket-config.el --- Configure Racket for Emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (use-package el-patch
;;   :ensure (el-patch :type git :host github :repo "radian-software/el-patch"
;;                       :fork (:host github
;;                              :repo "your-name/el-patch")))

(require 'magit)
(require 'lispy-config)
(require 'yasnippet-config)

(use-package racket-mode
  :ensure `(racket-mode :type git :host github :repo "greghendershott/racket-mode"
                        :fork (:host github
                                     :repo "KarlJoad/racket-mode"))
  :hook ((racket-mode . racket-xp-mode)
         (racket-mode . racket-unicode-input-method-enable)
         (racket-repl-mode . racket-unicode-input-method-enable)
         (racket-mode . (lambda () (set-input-method 'TeX))))
  :bind (:map racket-mode-map
         ("C-c C-z" . racket-run-and-switch-to-repl)))

(provide 'racket-config)
;;; racket-config.el ends here
