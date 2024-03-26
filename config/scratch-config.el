;;;; This file provides changes to the scratch buffer, as I need them -*- lexical-binding: t -*-
(provide 'personal-scratch)

;;; Initial messsage presented to the user in the *scratch* buffer.
;;; Keep in mind, these are just language-dependent comments, nothing special.
;;; i.e., Python/LaTeX = #, C/Java/C++/C#/etc. = // or /**/
(setq initial-scratch-message "\
;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.")

;; Changing the Initial Major mode of the *scratch* buffer
; Comment out the next line if I think that the *scratch* buffer should be in a different major mode
(setq initial-major-mode 'lisp-interaction-mode) ;; The default state for *scratch* work

;; The major modes that I primarily work with, in no particular order
;(setq initial-major-mode 'latex-mode)
;(setq initial-major-mode 'c-mode)
;(setq initial-major-mode 'java-mode)
;(setq initial-major-mode 'python-mode)
;(setq initial-major-mode 'lisp-mode)
;(setq initial-major-mode 'emacs-lisp-mode)
;(setq initial-major-mode 'ruby-mode)
