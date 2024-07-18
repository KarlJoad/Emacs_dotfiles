;;; gnuplot-config.el --- Configure gnuplot -*- lexical-binding: t -*-
;;; Commentary:
;;
;; gnuplot is a scriptable and interactive data plotting program. In many ways,
;; Python's matplotlib is quite similar to gnuplot.
;; I choose to use gnuplot becuase of its smaller footprint as a package. In
;; Guix, gnuplot is less than 1/3 of the size compared to what you need to make
;; matplotlib actually usable in Python!
;; This small size lends itself well to Guix Workflow Language too, as it means
;; the built environments and/or containers are smaller, build quicker, and
;; hopefully launch faster.

;;; Code:

(use-package gnuplot
  :ensure t
  :defer t
  ;; We must manually modify auto-mode-alist with the :mode portion so that
  ;; opening a gnuplot file automatically uses gnuplot-mode.
  :mode ("\\.gp$" . gnuplot-mode))

(provide 'gnuplot-config)
;;; gnuplot-config.el ends here
