;;; yasnippet-snippets-config.el --- Provides and configures the provided snippets
;;; Commentary:
;;
;; yasnippet-snippets provides a large amount of snippets for yasnippet to work with.
;; There are an innumerable amount of snippets for many, many languages.
;; This provides a good starting set of yasnippet snippets to use for most major modes.
;;
;;; Code:

(use-package yasnippet-snippets
  :after yasnippet
  ; :ensure t
  :config (yasnippet-snippets-initialize))

(provide 'yasnippet-snippets-config)
;;; yasnippet-snippets-config.el ends here
