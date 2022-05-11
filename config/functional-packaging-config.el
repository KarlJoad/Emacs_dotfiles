;;; functional-packaging-config.el --- Set up for work with functional packaging in Emacs
;;; Commentary:
;;
;; Functional package managers take their key concepts from functional programming
;; languages, the key one being immutability of data. If manipulations to data
;; are performed, then the operation must NOT destroy the original data. In this
;; case, destruction is in-place overwriting of the original data with the new.
;;
;; Nix and Guix take these concepts to the package management world to uniquely
;; identify every package needed to build every other package through the use of
;; a cryptographic hash (SHA3-256) which has been output in base32. The hash is
;; prepended to the front of the package name, giving a format like:
;; aa0a44kyrgrhb5mdq2r9pm939c14m4l0-texlive-universa-2.1
;; For digging into the store (the location where all these packages are stored)
;; having this hash is invaluable. But, for less in-depth use, the hash is just
;; annoying.
;;
;; This configuration file is intended to make working with these functional
;; packaging systems less painful inside Emacs.
;;
;;; Code:

(use-package pretty-sha-path :defer t :straight t)
(global-pretty-sha-path-mode)
;; Can turn off the pretty-sha-path-mode on a buffer-by-buffer basis by using the
;; non-global variant of the command.

(provide 'functional-packaging-config)
;;; functional-packaging-config.el ends here
