;;; erc-config.el --- This file provides my configuration for ERC
;;; Commentary:
;; ERC is the Emacs major mode for IRC conversations and channels.
;;; Code:

(require 'erc)

(erc-match-mode 1)
(add-to-list 'erc-keywords "\\KarlJoad\\b")
(add-to-list 'erc-keywords "\\KarlJoad`\\b")

(setq  erc-server-coding-system '(utf-8 . utf-8))


(provide 'erc-config)
;;; erc-config.el ends here
