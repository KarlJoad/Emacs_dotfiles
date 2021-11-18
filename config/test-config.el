;;; test-config.el --- Settings for making Test(s) workable
;;; Commentary:
;;; Code:

(use-package vertico
  :straight t
  :init (vertico-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(use-package marginalia
  :init (marginalia-mode)
  :bind (("M-A" . marginalia-cycle)))

;; (use-package consult)
;; (setq completion-in-region-function #'consult-completion-in-region)

;; (use-package embark)
;; (use-package embark-consult)
;; (global-set-key (kbd "C-s-a") #'embark-act)

(provide 'test-config)
;;; test-config.el ends here
