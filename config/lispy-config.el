;;; lispy-config.el --- Settings for making Lisp(s) workable
;;; Commentary:
;;
;; A lot of Lisps have a lot of things in common (I am looking at you
;; parentheses). Having a file with common configuration for ALL Lisps, whether
;; that be Scheme, Common Lisp, Clojure, or Emacs Lisp, will make the entire
;; sexperience that much more fluid.
;;
;;; Code:

;; Highlight different indentation levels of delimiters.
(require 'rainbow-delimiters-config)

;; Make editing sexps easier
(use-package paredit
  :defer t
  :straight t)

(provide 'lispy-config)
;;; lispy-config.el ends here
