;;; email-config.el --- This file configured mu4e for my email
;;; Commentary:
;;
;; Since I tend to use GMail for many things, I have to perform multiple steps
;; to use Emacs to receive, read, compose, and send emails.
;;
;; First, we need an IMAP mail fetching program.
;; For OpenSUSE Tumbleweed, I build my own isync package.
;;   This program contains mbsync as its main binary; project name is isync.
;;   The configuration file for this program is ~/.mbsyncrc
;;   You need libopenssl-devel and cyrus-sasl-devel packages from OpenSUSE.
;;
;; Next, we need a way to index our mail so we can search through it.
;; mu4e requires another program be installed that interfaces with IMAP
;; to download the emails, since mu ONLY indexes and queries the downloaded
;; emails.
;;
;; Lastly, we need a way to send the email through SMTP.
;; I will use msmtp to send my mail.
;; It requires there be an ~/.msmtprc config file.
;; Special permissions are required, namely 600.
;;; Code:

;; Add the path to the mu4e source code
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(require 'mu4e)

(setq mu4e-maildir "~/Mail")

(setq mu4e-contexts
      `(,(make-mu4e-context
          :name "Personal"
          :match-func (lambda (msg)
                        (when msg
                          (string-prefix-p "/Personal" (mu4e-message-field msg :maildir))))
          :vars '(;; (user-full-name "Karl G. Hallsby") ;; My full name is set in personal-info
				  ;; (user-mail-address . "karl@hallsby.com") ;; My personal email address is set in personal-info
                  (mu4e-trash-folder . "/Personal/archive")
                  (mu4e-refile-folder . "/Personal/archive")
                  (mu4e-sent-folder . "/Personal/sent")
                  (mu4e-drafts-folder . "/Personal/drafts")))))

(setq mu4e-change-filenames-when-moving t) ;; Rename files when moving them between directories

;; Shamelessly stolen from Howard R. Schwarz's configuration.org file.
(defun karljoad/encrypt-responses ()
  "Encrypt the current message if it's a reply to another encrypted message."
  (let ((msg mu4e-compose-parent-message))
    (when (and msg (member 'encrypted (mu4e-message-field msg :flags)))
        (mml-secure-message-encrypt-pgpmime))))

(add-hook 'mu4e-compose-mode-hook 'hrs/encrypt-responses)

(provide 'email-config)
;;; email-config.el ends here
