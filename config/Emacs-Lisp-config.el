;;; Emacs-Lisp-config.el --- This is file provides my personal changes to the Emacs-List major mode
;;; Commentary:
;;; Code:

;; Use the AucTeX keybinding to comment out a region
;; This will only apply to buffers with the Emacs-Lisp major mode
(local-set-key (kbd "C-c ;") 'comment-or-uncomment-region)

(provide 'Emacs-Lisp-config)
;;; Emacs-Lisp-config.el ends here
