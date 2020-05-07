;;; early-init.el --- Emacs 27+ pre-initialisation config

;;; Commentary:

;; Emacs 27+ loads this file before (normally) calling
;; `package-initialize'.  We use this file to suppress that automatic
;; behaviour so that startup is consistent across Emacs versions.

;;; Code:

(setq package-enable-at-startup nil)

(defvar karljoad-gc-cons-threshold (* 4 1024 1024) "Karl's choice of the amount of memory used before GC is run. 4MB.")
(defvar karljoad-gc-cons-percentage 0.5 "Karl's choice of the percent memory used before GC is run.")
(defvar karljoad-message-log-max (* 16 1024) "Maximum number of messages to keep in *Messages* buffer.")

(setq-default gc-cons-threshold karljoad-gc-cons-threshold
              gc-cons-percentage karljoad-gc-cons-percentage
              message-log-max karljoad-message-log-max)

;; So we can detect this having been loaded
(provide 'early-init)

;;; early-init.el ends here
