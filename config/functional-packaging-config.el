;;; functional-packaging-config.el --- Set up for work with functional packaging in Emacs
;;; Commentary:
;;; Code:

(use-package pretty-sha-path :defer t :straight t)
(global-pretty-sha-path-mode)
;; Can turn off the pretty-sha-path-mode on a buffer-by-buffer basis by using the
;; non-global variant of the command.

(provide 'functional-packaging-config)
;;; functional-packaging-config.el ends here
