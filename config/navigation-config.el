;;; navigation-config.el --- Ease navigation through Emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package vertico
  :straight t
  :init (vertico-mode)
  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(use-package emacs
  :straight (:type built-in)
  :hook (((minibuffer-setup-hook) . #'cursor-intangible-mode))
  :custom
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (enable-recursive-minibuffers t))

;; Show information in minibuffer about function/variable and keybinding
(use-package marginalia
  :init (marginalia-mode)
  :bind (("M-A" . marginalia-cycle)))

(provide 'navigation-config)
;;; navigation-config.el ends here
