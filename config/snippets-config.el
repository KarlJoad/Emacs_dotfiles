;;; snippets-config.el --- Provides and configure snippets -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Programming snippets are templates that provide a way for a short
;; abbreviation to expand into a whole body of code while leaving blank spots
;; for your cursor/point to jump to.
;;
;; This provides the yasnippet package, which allows me to auto-generate a
;; certain control flow. It can be a loop, a conditional, whatever. It allows me
;; to quickly and easily insert snippets of text Snippets are expanded by typing
;; enough of it in, and them pressing <TAB> to enter it. It is important to note
;; that this IS NOT the little dropdown that I get while typing. That's provided
;; by your chosen completion framework. Yasnippet's snippets are predefined in
;; their respective file(s).
;;
;;; Code:

(use-package tempel
  :ensure t
  :defer t
  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))

  :init
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.  `tempel-expand'
    ;; only triggers on exact matches. We add `tempel-expand' *before* the main
    ;; programming mode Capf, such that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand completion-at-point-functions))

    ;; Alternatively use `tempel-complete' if you want to see all matches.  Use
    ;; a trigger prefix character in order to prevent Tempel from triggering
    ;; unexpectly.
    ;; (setq-local corfu-auto-trigger "/"
    ;;             completion-at-point-functions
    ;;             (cons (cape-capf-trigger #'tempel-complete ?/)
    ;;                   completion-at-point-functions))
    )

  ;; (add-hook 'conf-mode-hook #'tempel-setup-capf)
  (add-hook 'prog-mode-hook #'tempel-setup-capf)
  ;; (add-hook 'text-mode-hook #'tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
  )

;;; 
;; tempel-collection contains some ready-made templates.

(use-package tempel-collection
  :ensure t
  :after (tempel))

(provide 'snippets-config)
;;; snippet-config.el ends here
