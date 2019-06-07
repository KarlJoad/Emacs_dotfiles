;;;; This file provides my personal tweaks to Emacs
(provide 'personal-settings)

;;;; Make sure Emacs loads up newer files, even if they aren't compiled yet
(setq load-prefer-newer t)

;;;; Remove scroll bar at side
(scroll-bar-mode -1)

;;;; Skip the "Welcome" Page
;(setq inhibit-startup-message t)

;;;; Turn on Line numbering
(global-display-line-numbers-mode)

(show-paren-mode) ; Emphasize Parentheses
(setq blink-matching-paren nil) ; But don't let them blink
