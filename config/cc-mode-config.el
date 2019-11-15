;;; cc-mode-config.el --- Config for C/C++/Java editing
					;-*-Emacs-Lisp-*-
;;; Commentary:
;;; Code:

(use-package ccls)

(add-hook 'c++-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c C-c") 'compile)))

(setq-default c-default-style "stroustrup"
	      c-basic-offset 4
	      tab-width 4)

;; setup GDB
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t
 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t)

(provide 'cc-mode-config)
;;; cc-mode-config.el ends here
