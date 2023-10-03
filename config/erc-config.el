;;; erc-config.el --- This file provides my configuration for ERC
;;; Commentary:
;; ERC is the Emacs major mode for IRC conversations and channels.
;;; Code:

(require 'erc)

(erc-match-mode 1)
(add-to-list 'erc-keywords "\\KarlJoad\\b")
(add-to-list 'erc-keywords "\\KarlJoad`\\b")
(add-to-list 'erc-keywords "\\KarlJoad-\\b")

(setq erc-server-coding-system '(utf-8 . utf-8))

(setq erc-server "irc.libera.chat"
      erc-port 6697
	    erc-nick "RavenJoad"
	    erc-user-full-name "Raven"
	    erc-track-shorten-start 8 ; Show first 8 characters of channel name with activity
	    erc-autojoin-channels-alist '(("irc.libera.chat" "#guix" "#guile" "#spritely"
                                     "#nixos"))
	    erc-kill-buffer-on-part t ; Kill a channel's buffer when I part from it.
      erc-modules
      '(button completion fill irccontrols match menu readonly ring spelling
               stamp notifications networks))

;; ERC can rename server-connection buffers to just the network's name.
;; This is useful when using an IRC bouncer, like ZNC, which has multiple
;; connections to the same server.
(setq erc-rename-buffers t)

(use-package erc-hl-nicks
  :after erc
  :config (add-to-list 'erc-modules 'hl-nicks))


(provide 'erc-config)
;;; erc-config.el ends here
