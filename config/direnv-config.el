;;; direnv-config.el --- This file provides my personal information
;;; Commentary:
;;
;; Set up Emacs to work with direnv's dynamic changing of environment variables.
;; The changing of environment variables is actually handled by direnv, but lorri
;; will also allow for the packages specified in default.nix or shell.nix to be
;; rebuilt in the store if need be.
;;
;; lorri is only available for Nix-based systems (NixOS, Linux with Nix, MacOS
;; with Nix). But, I won't run the second 2 options, so I check if this is NixOS.
;;
;;; Code:

(use-package direnv)

;; Global minor mode to provide the same functionality as the direnv-update-*
;; functions.
(direnv-mode)

;; Bind direnv-allow (equivalent of shell's direnv allow) to a key chord.
(global-set-key (kbd "C-c d a") 'direnv-allow)

;; Update Emacs environment to the direnv environment for the current file
(global-set-key (kbd "C-c d u") 'direnv-update-environment)

;; Update Emacs to the direnv environment for the current buffer (doesn't have
;; to be associated with a file).
(global-set-key (kbd "C-c d e") 'direnv-update-directory-environment)

;; Always show summary message in minibuffer.
;; Contains list of automatic changes taken by direnv.
;; Output can be suppressed by setting `direnv-always-show-summary' to nil.
;; Interactive calls will still show a summary message.
(setq direnv-always-show-summary t)

;; Whether summary message should contain paths of the old and new directories
(setq direnv-show-paths-in-summary nil)

;; Let the summary message use different font faces for: added, changed, and
;; removed environment variables.
;; Depending on theme, this may result in different colors.
(setq direnv-use-faces-in-summary t)

(provide 'direnv-config)
;;; direnv-config.el ends here
