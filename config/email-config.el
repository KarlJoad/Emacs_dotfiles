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

;; Give myself a nice easy keybinding to open mu4e
(global-set-key (kbd "C-c m") 'mu4e)

;; If there are web links in an email, open them in my default browser
;; This uses the same keybinding as org-mode, making it easy to remember.
(define-key mu4e-view-mode-map (kbd "C-c C-o") 'mu4e~view-browse-url-from-binding)

;; HTML email is rife in the world. It is used by Gmail, for instance.
;; There are accessibility reasons why not to use it, but I still want to be able
;;  to read emails sent through Gmail. So, we configure that here.
(require 'mu4e-contrib)
(setq mu4e-html2text-command 'mu4e-shr2text
      shr-color-visible-luminance-min 60
      shr-color-visible-distance-min 5
      shr-use-fonts nil
      shr-use-colors nil)
(advice-add #'shr-colorize-region
            :around (defun shr-no-colourise-region (&rest ignore)))

;; However, there are some HTML emails that are just too hard for Emacs to display.
;; So, open the HTML up in my browser.
;; By default, this is bound to "a h" in the mu4e mode.
(add-to-list 'mu4e-view-actions
             '("HTML in Browser" . mu4e-action-view-in-browser)
			 ;; Append the action, to list, rather than overwrite.
			 ;; The add-to-list function actually appends to the FRONT of the list!
             t)

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

;; Rename files when moving them between directories
(setq mu4e-change-filenames-when-moving t)

;; Don't show the context of a thread in the Inbox, once it has been deleted
(setq mu4e-headers-include-related nil)

;; Show the email address of the person I am emailing along with their name.
(setq mu4e-view-show-addresses t)


;; Shamelessly stolen from Howard R. Schwarz's configuration.org file.
(defun karljoad/encrypt-responses ()
  "Encrypt the current message if it's a reply to another encrypted message."
  (let ((msg mu4e-compose-parent-message))
    (when (and msg (member 'encrypted (mu4e-message-field msg :flags)))
        (mml-secure-message-encrypt-pgpmime))))

(add-hook 'mu4e-compose-mode-hook 'karljoad/encrypt-responses)

(provide 'email-config)
;;; email-config.el ends here
