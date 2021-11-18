;;; reftex-config.el --- This is file provides my personal changes to the RefTeX package
;;; Commentary:
;;
;; Reftex comes installed with Emacs, so we just use require to pull it in.
;;
;;; Code:

(require 'latex)
;; Make sure that reftex gets pulled in when Emacs loads
(require 'reftex) ;; RefTeX comes with Emacs

;; Make RefTeX play nice with AucTeX
(setq reftex-plug-into-AUCTeX t)

;; Make sure that reftex gets loaded when AucTeX gets loaded, i.e. when LaTeX file is opened
(add-hook 'LaTeX-mode-hook
          #'turn-on-reftex)

;; Scan the whole document for new labels/citations
(define-key LaTeX-mode-map (kbd "C-c r") #'reftex-parse-all)

;; When parsing very large documents, we might not want to reparse every file
(setq reftex-enable-partial-scans t)

;; Set a default citation style for RefTeX to use
(setq reftex-cite-format 'biblatex)

;; Set a default style to present possible citation matches
(setq reftex-sort-bibtex-matches 'author)

(provide 'reftex-config)
;;; reftex-config.el ends here
