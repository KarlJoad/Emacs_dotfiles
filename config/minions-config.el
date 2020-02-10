;;; minions-config.el --- This file provides my personal information
;;; Commentary:
;;
;; The minions package removes all the additional minor-mode names
;; and their information from the mode-line.
;; If I have them all showing, the modeline gets very busy, and very
;; hard to read sometimes.
;; So, I use this package to remove them, leaving only the current
;; major-mode and a ;-) for the rest of the minor modes.
;;
;;; Code:

(use-package minions
  :config (minions-mode 1))

(provide 'minions-config)
;;; minions-config.el ends here
