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

(provide 'reftex-config)
;;; reftex-config.el ends here
