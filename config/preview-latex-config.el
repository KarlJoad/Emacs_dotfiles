;;; preview-latex-config.el --- Provides and changes the preview-latex package
;;; Commentary:
;;
;; preview-latex is a package that generates portions of a TeX/LaTeX document on the fly.
;; This allows us to view equations as they would be output in real time, among other things.
;;
;;; Code:

(setq preview-image-type "png") ;; Default Preview is png

(setq preview-auto-cache-preamble t) ;; Cache the preamble before running the first time, helpful for large preambles

(setq preview-auto-reveal t) ;; Automatically preview the results

(provide 'preview-latex-config)
;;; preview-latex-config.el ends here
