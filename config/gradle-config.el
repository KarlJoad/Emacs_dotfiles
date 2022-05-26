;;; gradle-config.el --- Settings for integrating the Gradle build tool with Emacs
;;; Commentary:
;;; Code:

(use-package gradle-mode)

(require 'gradle-mode)
(add-hook 'java-mode-hook #'(lambda() (gradle-mode 1)))

(provide 'gradle-config)
;;; gradle-config.el ends here
