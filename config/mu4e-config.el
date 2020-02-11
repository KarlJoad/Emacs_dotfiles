;;; mu4e-config.el --- This file configured mu4e for my email
;;; Commentary:
;;
;; mu4e requires another program be installed that interfaces with IMAP
;; to download the emails, since mu ONLY indexes and queries the downloaded
;; emails.
;;
;; For OpenSUSE Tumbleweed, I use the repository's isync package.
;;   This program contains mbsync as its main binary; project name is isync.
;;   The configuration file for this program is ~/.mbsyncrc
;; For other distributions, I may use OfflineIMAP.
;;
;;; Code:

;; Add the path to the mu4e source code
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(require 'mu4e)


;; Shamelessly stolen from Howard R. Schwarz's configuration.org file.
(defun karljoad/encrypt-responses ()
  "Encrypt the current message if it's a reply to another encrypted message."
  (let ((msg mu4e-compose-parent-message))
    (when (and msg (member 'encrypted (mu4e-message-field msg :flags)))
        (mml-secure-message-encrypt-pgpmime))))

(add-hook 'mu4e-compose-mode-hook 'hrs/encrypt-responses)

(provide 'mu4e-config)
;;; mu4e-config.el ends here
