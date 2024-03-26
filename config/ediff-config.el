;;; ediff-config.el --- Configure ediff -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package ediff
  :straight (:type built-in)
  :defer t
  :custom
  (ediff-keep-variants nil)
  (ediff-make-buffers-readonly-at-startup 't)
  (ediff-merge-revisions-with-ancestor 't)
  (ediff-show-clashes-only 't)
  ;; ediff splits should be made horizontally, producing several vertical buffers
  (ediff-split-window-function #'split-window-horizontally)
  ;; Keep ediff control buffer in the same frame as the diff-ed contents
  (ediff-window-setup-function #'ediff-setup-windows-plain))

(provide 'ediff-config)
;;; ediff-config.el ends here
