;;;; This is file provides my personal changes to the RefTeX package
(provide 'reftex-config)

;; Make sure that reftex gets pulled in when Emacs loads
(require 'reftex)

;; Make RefTeX play nice with AucTeX
(setq reftex-plug-into-AUCTeX t)

;; Make sure that reftex gets loaded when AucTeX gets loaded, when LaTeX file is opened
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
