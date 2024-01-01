;;; dired-config.el --- Configure dired
;;; Commentary:
;;; Code:

;; C-x C-j is bound to the (dired-jump) command by default. Put on the more
;; obvious C-x d.
;; We leave the more powerful, but verbose, (dired) command on C-x D
;; It is safe to use keymap-global-set here because these dired commands are set
;; in the global-map.
(keymap-global-unset "C-x C-j")
(keymap-global-set "C-x d" #'dired-jump)
(keymap-global-set "C-x D" #'dired)

(provide 'dired-config)
;;; dired-config.el ends here
