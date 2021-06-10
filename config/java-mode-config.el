;;; java-mode-config.el --- Settings for making Emacs work in Java-mode
;;; Commentary:
;;; Code:

(add-hook 'java-mode-hook (lambda ()
			    (setq c-basic-offset 4
				 tab-width 4
				 indent-tabs-mode nil)))
;; By setting indent-tabs-mode to nil, when I press <TAB>, I insert 4 spaces instead

(use-package posframe)
(use-package company-posframe)

;; Setup LSP for java.
(use-package lsp-mode
  ; :ensure t
  :hook (
	 (java-mode . #'lsp-deferred))
  :commands lsp)

(use-package lsp-java
  ; :ensure t)

(add-hook 'java-mode-hook #'lsp)
(add-hook 'java-mode-hook 'flycheck-mode)
(add-hook 'java-mode-hook 'company-mode)
(add-hook 'java-mode-hook '(lambda() (gradle-mode 1)))

(use-package gradle-mode)
(defun build-and-run ()
  "Build the Java application with gradle, then run the resulting program."
  (interactive)
  (gradle-run "build run"))
; TODO: The line below binds "C-c C-c" for EVERY major mode, not just gradle/java
; (define-key gradle-mode-map (kbd "C-c C-c") 'build-and-run)

(require 'company)
(use-package company-emacs-eclim)
(company-emacs-eclim-setup)

;; Debugger Adapter Protocol, for debugging java code
(use-package dap-java
  ; :ensure nil)

(use-package dap-mode
  ; :ensure t
  :after (lsp-mode)
  :functions dap-hydra/nix
  :config
  (require 'dap-java))

(provide 'java-mode-config)
;;; java-mode-config.el ends here
