;;; rust-config.el --- Handles everything needed for Rust source code development
;;; Commentary:
;;; Code:

(require 'magit)

;; rust-mode is developed by The Rust Language makers, but not actively maintained
;; (use-package rust-mode)

;; Therefore, I will use the rustic package for Rust development instead
(use-package rustic) ;; Development environment for Rust

(provide 'rust-config)
;;; rust-config.el ends here
