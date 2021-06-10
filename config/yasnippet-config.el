;;; yasnippet-config.el --- Provides and configure yasnippet
;;; Commentary:
;;
;; This provides the yasnippet package, which allows me to auto-generate a certain control flow.
;; It can be a loop, a conditional, whatever.  It allows me to quickly and easily insert snippets of text
;; Snippets are expanded by typing enough of it in, and them pressing "<tab>" to enter it.
;; It is important to note that this IS NOT the little dropdown that I get while typing.
;; That's provided by company.  Snippets are predefined in their respective file(s).
;;
;;; Code:

(use-package yasnippet
  ; :ensure t
  :defer t
  :config
  (yas-reload-all)
  (defvar yas-snippet-dirs (list (concat user-emacs-directory "snippets") yasnippet-snippets-dir) "List of directories to find snippets for the yasnippet package")
  (setq tab-always-indent 'complete) ;; Tabs indent
  (setq yas-prompt-functions '(yas-completing-prompt ;; List of prompts that yasnippet can go through
			       yas-ido-prompt
			       yas-dropdown-prompt))
  (define-key yas-minor-mode-map (kbd "<escape>") 'yas-exit-snippet)) ;; If there was a menu for selecting snippets, provide an escape route

(yas-global-mode 1) ;; Make sure yasnippet is almost always present

(provide 'yasnippet-config)
;;; yasnippet-config.el ends here
