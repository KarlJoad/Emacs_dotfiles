;;; rainbow-delimiters-config.el --- This file configures rainbow-delimiters
;;; Commentary:
;;
;; This is NOT to be confused with rainbow-mode.
;; This package will rainbow-color the parentheses/brackets/braces/angles in a buffer,
;; making it MUCh easier to see how deeply nested I am and to easily find where I am.
;;
;;; Code:

;; Color nested parentheses/brackets/braces successively
(use-package rainbow-delimiters
  :straight t)
;; (straight-use-package 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(provide 'rainbow-delimiters-config)
;;; rainbow-delimiters-config.el ends here
