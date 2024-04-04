;;; prog-mode-config.el --- General configuration for programming modes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;;
;;; Movement

(defun backward-symbol (arg)
  "Move point to the previous position that is the beginning of a symbol.
A symbol is any sequence of characters that are in either the
word constituent or symbol constituent syntax class.
With prefix argument ARG, do it ARG times if positive, or move
forward ARG times if negative."
  (interactive "^p")
  (forward-symbol -1))

;; When moving over code, you do not care about the actual words within a
;; symbol in the language, instead you want to move over the symbol. For example,
;; in C, variable_a is a symbol. If the point (cursor) is before the symbol name,
;; then forward-word would move the point right after "variable", but before the
;; "_". Instead, I would prefer to move forward by the whole symbol, placing point
;; at the end of the symbol instead.
(add-hook 'prog-mode-hook
          (lambda ()
            (local-set-key (kbd "M-f") #'forward-symbol)
            (local-set-key (kbd "M-b") #'backward-symbol)))

(provide 'prog-mode-config)
;;; prog-mode-config.el ends here
