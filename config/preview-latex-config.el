;;;; This file provides my personal changes to the preview-latex package
(provide 'preview-latex-config)

;;; My settings go here

(setq preview-image-type "png") ; Default Preview is png

(setq preview-auto-cache-preamble t) ; Cache the preamble before running the first time, helpful for large preambles

(setq preview-auto-reveal t) ; Automatically preview the results
