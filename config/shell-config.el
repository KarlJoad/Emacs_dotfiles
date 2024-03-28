;;; shell-config.el --- Configure Emacs to understand shells -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'shell)
(require 'dirtrack)
(require 'comint)

;; Help Emacs track the current directory of the shell by printing an OSC 7
;; URI-style directory information in the shell's prompt.
;; https://www.masteringemacs.org/article/running-shells-in-emacs-overview#directory-tracking
;; NOTE: Emacs 28 was the first version to add support for this tracking
(when (> emacs-major-version 28)
  ;; Disable shell-dirtrack-mode and dirtrack-mode because they interfere with
  ;; comint's ability to track with OSC 7 URIs.
  (shell-dirtrack-mode 0)
  (dirtrack-mode 0)
  (add-hook 'comint-output-filter-functions #'comint-osc-process-output))

(provide 'shell-config)
;;; shell-config.el ends here
