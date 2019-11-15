;;; reftex-config.el --- This is file provides my personal changes to the RefTeX package
;;; Commentary:
;;
;; Reftex comes installed with Emacs, so we just use require to pull it in.
;;
;;; Code:

;; Make sure that reftex gets pulled in when Emacs loads
(require 'reftex)

;; Make RefTeX play nice with AucTeX
(setq reftex-plug-into-AUCTeX t)

;; Make sure that reftex gets loaded when AucTeX gets loaded, i.e. when LaTeX file is opened
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

;; When parsing very large documents, we might not want to reparse every file
(setq reftec-enable-partial-scans t)

;; Set a default citation style for RefTeX to use
(setq reftex-cite-format 'biblatex)

;; Set a default style to present possible citation matches
(setq reftex-sort-bibtex-matches 'author)

;; Make a local keybinding for the RefTeX minor mode
(local-set-key (kbd "C-c r") 'reftex-parse-all) ;; Scan the whole document for new labels/citations
(local-set-key (kbd "C-c f") 'reftex-goto-label) ;; Jump to \ref{}'s \label definition

;; RefTeX comes with a set of reference styles where each relates to one or more reference macros.
;; In order to configure a preference permanently, the variable reftex-ref-style-default-list should be customized
;; (add-to-list 'reftex-ref-style-default-list)
;; (add-to-list 'reftex-ref-style-alist)

;; Example for the above reftex-ref-style-default-list and reftex-ref-style-alist
;; (eval-after-load
;;     "latex"
;;   '(TeX-add-style-hook
;;     "cleveref"
;;     (lambda ()
;;       (if (boundp 'reftex-ref-style-alist)
;;       (add-to-list
;;        'reftex-ref-style-alist
;;        '("Cleveref" "cleveref"
;;          (("\\Cref" ?C) ("\\cref" ?c) ("\\cpageref" ?d) ("\\Cpageref" ?D)))))
;;       (add-to-list 'reftex-ref-style-default-list "Cleveref")
;;       (setq reftex-label-alist '(AMSTeX)) ; not sure why it doesn't parse amstex
;;                                         ; automatically
;;       (TeX-add-symbols
;;        '("cref" TeX-arg-ref)
;;        '("Cref" TeX-arg-ref)
;;        '("cpageref" TeX-arg-ref)
;;        '("Cpageref" TeX-arg-ref)))))

(provide 'reftex-config)
;;; reftex-config.el ends here
