;;; company-config.el --- Provides and configures company-mode
;;; Commentary:
;;
;; Provides and configures CompleteAny-mode
;; company-mode is just the front end for the auto-completion.
;; There MUST be a backend for company-mode to run off of.
;; C/C++ requires Clang
;; Elisp requires Emacs, etc.
;;
;;; Code:

;; Company gives us an engine for some auto-completion for things we provide a backend for
(use-package company
  :straight t
  :defer t
  :init
  (global-company-mode)
  :config
  (setq company-idle-delay 0)
  (define-key company-active-map (kbd "<escape>") 'company-abort) ;; Give myself an escape route for suggestions.
  (define-key company-active-map [tab] 'company-complete-common-or-cycle) ;; Use TAB to select the next suggestion, or to cycle to the next option.
  ;; Which one is chosen depends on if the completion is unique.
  (define-key company-active-map (kbd "C-n") 'company-select-next) ;; Use C-n to select the next suggestion
  (define-key company-active-map (kbd "C-p") 'company-select-previous) ;; Use C-p to select previous suggestion
  (define-key company-active-map (kbd "C-d") 'company-show-doc-buffer) ;; Use C-d to show the documentation for the thing
  (define-key company-active-map (kbd "C-l") 'company-show-location) ;; Use C-l to show the location of the suggestion that Company is giving you right now
  )

(provide 'company-config)
;;; company-config.el ends here
