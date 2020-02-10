;;; early-init.el --- Emacs 27+ pre-initialization config
;;; Commentary:
;;
;; This file is loaded before ANY package initialization occurs.
;; So, this is exactly the spot where we want to modify the GC
;; properties of Emacs, allowing it start more quickly.
;;
;;; Code:

;; Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(provide 'early-init)
;;; early-init.el ends here
