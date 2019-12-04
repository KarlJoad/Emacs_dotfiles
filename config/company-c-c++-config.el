;;; company-c-c++-config.el --- Configuration for company for C/C++ development
					;-*-Emacs-Lisp-*-
;;; Commentary:
;;; Code:

(use-package company-c-headers
  :init
  (add-to-list 'company-backends 'company-c-headers))

(use-package company-ctags
  :ensure t
  :defer t)

(use-package company-lsp)

;; Available C style:
;; “gnu”: The default style for GNU projects
;; “k&r”: What Kernighan and Ritchie, the authors of C used in their book
;; “bsd”: What BSD developers use, aka “Allman style” after Eric Allman.
;; “whitesmith”: Popularized by the examples that came with Whitesmiths C, an early commercial C compiler.
;; “stroustrup”: What Stroustrup, the author of C++ used in his book
;; “ellemtel”: Popular C++ coding standards as defined by “Programming in C++, Rules and Recommendations,” Erik Nyquist and Mats Henricson, Ellemtel
;; “linux”: What the Linux developers use for kernel development
;; “python”: What Python developers use for extension modules
;; “java”: The default style for java-mode (see below)
;; “user”: When you want to define your own style
(setq c-default-style "linux") ;; set style to "linux"

(use-package cc-mode
  :init
  (define-key c-mode-map  (kbd "C-c SPC") 'company-complete)
  (define-key c++-mode-map  (kbd "C-c SPC") 'company-complete))

(provide 'company-c-c++-config)
;;; company-c-c++-config.el ends here
