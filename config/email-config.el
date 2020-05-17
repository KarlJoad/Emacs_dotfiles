;;; email-config.el --- This file configured mu4e for my email
;;; Commentary:
;;
;; Since I tend to use GMail for many things, I have to perform multiple steps
;; to use Emacs to receive, read, compose, and send emails.
;;
;; First, we need an IMAP mail fetching program.
;; For OpenSUSE Tumbleweed, I build my own isync package.
;;   This program contains mbsync as its main binary; project name is isync.
;;   The configuration file for this program is `~/.mbsyncrc'
;;   You need libopenssl-devel and cyrus-sasl-devel packages from OpenSUSE.
;;
;; Next, we need a way to index our mail so we can search through it.
;; mu4e requires another program be installed that interfaces with IMAP
;; to download the emails, since mu ONLY indexes and queries the downloaded
;; emails.
;;
;; To start using `mu' (maildir-utils), you must first download the mail using isync/`mbsync'.
;; Then, you must perform an `mu init --maildir=<path-to-maildir> --my-address="example@domain.com" [--my-address="example2@domain2.com"]'
;; You only need to initialize the mu database once, but you can feed it several personal addresses when initializing.
;; Once done, you must index the database with `mu index'
;;
;; Lastly, we need a way to send the email through SMTP.
;; I will use msmtp to send my mail.
;; It requires there be an `~/.msmtprc' config file.
;; Special permissions are required, namely 600.
;;
;;; Code:

;; Add the path to the mu4e source code
;; This is placed here when mu is installed.
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(require 'mu4e)

;; The location of my mail for ALL of the accounts
;; (setq mu4e-maildir "~/Mail")
;; As of mu version 1.4.
;; `mu4e' no longer uses the `mu4e-maildir' and instead it uses the information
;; it gets from `mu'.
;; See the large comment above in the commentary.

;; Give myself a nice easy keybinding to open mu4e
(global-set-key (kbd "C-c m") 'mu4e)

;; mu4e starts REAL QUICK, so closing it isn't that bad.
;; Besides, I have a nice easy keybinding to open it quickly anyways.
(setq mu4e-confirm-quit nil)

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
          :name "personal"
          :match-func (lambda (msg)
                        (when msg
                          (string-prefix-p "/Personal" (mu4e-message-field msg :maildir))))
          :vars '(;; (user-full-name "Karl G. Hallsby") ;; My full name is set in personal-info
		  (user-mail-address . "karl@hallsby.com")
		  ;; Although personal email address set in personal-info, need to reset it
		  ;; when I change contexts in mu4e
                  (mu4e-trash-folder . "/Personal/Trash")
                  (mu4e-refile-folder . "/Personal/Refile")
                  (mu4e-sent-folder . "/Personal/Sent")
                  (mu4e-drafts-folder . "/Personal/Drafts")))
	,(make-mu4e-context
	  :name "iit"
	  :match-func (lambda (msg)
                        (when msg
                          (string-prefix-p "/Personal" (mu4e-message-field msg :maildir))))
          :vars '(;; (user-full-name "Karl G. Hallsby") ;; My full name is set in personal-info
		  (user-mail-address . "khallsby@hawk.iit.edu")
                  (mu4e-trash-folder . "/IIT/Trash")
                  (mu4e-refile-folder . "/IIT/Refile")
                  (mu4e-sent-folder . "/IIT/Sent")
                  (mu4e-drafts-folder . "/IIT/Drafts")))
	,(make-mu4e-context
	  :name "lund"
	  :match-func (lambda (msg)
                        (when msg
                          (string-prefix-p "/Lund" (mu4e-message-field msg :maildir))))
          :vars '(;; (user-full-name "Karl G. Hallsby") ;; My full name is set in personal-info
		  (user-mail-address . "ka1867ha-s@student.lu.se")
                  (mu4e-trash-folder . "/Lund/Trash")
                  (mu4e-refile-folder . "/Lund/Refile")
                  (mu4e-sent-folder . "/Lund/Sent")
                  (mu4e-drafts-folder . "/Lund/Drafts")))
	,(make-mu4e-context
	  :name "triangleserveradmin"
	  :match-func (lambda (msg)
                        (when msg
                          (string-prefix-p "/TriangleServerAdmin" (mu4e-message-field msg :maildir))))
          :vars '(;; (user-full-name "Karl G. Hallsby") ;; My full name is set in personal-info
		  (user-mail-address . "serveradmin@triangleiit.org")
                  (mu4e-trash-folder . "/TriangleServerAdmin/Trash")
                  (mu4e-refile-folder . "/TriangleServerAdmin/Refile")
                  (mu4e-sent-folder . "/TriangleServerAdmin/Sent")
                  (mu4e-drafts-folder . "/TriangleServerAdmin/Drafts")))))

(add-to-list 'mu4e-bookmarks
       (make-mu4e-bookmark
        :name "All Inboxes"
        :query "maildir:/Personal/Inbox OR maildir:/IIT/Inbox OR maildir:/Lund/Inbox OR maildir:/TriangleServerAdmin/Inbox"
        :key ?a))

;; We want to get ALL mail with the mbsync command with the -a flag.
(setq mu4e-get-mail-command "mbsync -a")

;; Rename files when moving them between directories
(setq mu4e-change-filenames-when-moving t)

;; When sending mail, delete the message file.
;; If using Gmail, messages are moved to the Sent folder by Google.
;; So, we don't need to do anything on our end.
(setq mu4e-sent-messages-behavior 'delete)

;; Don't show the context of a thread in the Inbox, once it has been deleted
(setq mu4e-headers-include-related nil)

;; Show the email address of the person I am emailing along with their name.
(setq mu4e-view-show-addresses t)

;; Since I use multiple email accounts, I want to make sure that I always send
;;  my emails from the correct mail account/context.
;; When composing a new email, always ask them, just to confirm.
(setq mu4e-compose-context-policy 'always-ask)

;; C-x m is bound by default with Emacs to the function compose-mail
;; It choses the mail composition package from mail-user-agent.
;; So, I set mail-user-agent to use the mu4e-user-agent.
;; This means I don't have to change the function the key is bound to, and I
;;  still get to use my preferred mail-composition package.
(setq mail-user-agent 'mu4e-user-agent)

;; Once I finish the email AND HAVE SENT IT, I want the buffer containing the mail
;; to be killed, rather than buried in the buffer-list.
(setq message-kill-buffer-on-exit t)

;; =============================================================================
;; Allow mu4e to use some capabilities of org-mode
;; =============================================================================
;; Enable org-mode style tables in messages
(add-hook 'message-mode-hook 'turn-on-orgtbl)

;; Enable org-mode like list manipulation
;; This may also include the section headers that org-mode uses
(add-hook 'message-mode-hook 'turn-on-orgstruct++)

;; =============================================================================
;; Mail sending setup
;; =============================================================================
;; Since I use Gmail, I have to use SMTP to send my emails.
;; This means I need to use a non-default mail sender, namely the program msmtp.

;; This will send ALL mail IMMEDIATELY, and will fail if you do not have an
;; Internet connection.
;; We set this by default here, so we can always try to send something
(setq sendmail-program "msmtp")
;; Or, we can queue them, and then have an mu4e keybinding to send them when we
;; get the chance.
(setq smtpmail-queue-mail nil ;; Switched by my4e~main-toggle-mail-sending-mode function
	  smtpmail-queue-dir "~/.msmtpqueue/") ;; What directory the queue is in

;; Overwrite the mu4e~main-toggle-mail-sending-mode keybinding with my own function
(define-key mu4e-main-mode-map (kbd "m") 'karljoad/set-sendmail-program)
(defun karljoad/set-sendmail-program ()
  "Set the smtpmail variable sendmail-program based on the value of smtpmail-queue-mail's value."
  (interactive)
  (mu4e~main-toggle-mail-sending-mode)
  (if smtpmail-queue-mail ;; Is true, meaning we queue it
	  (setq sendmail-program "msmtp-enqueue.sh")
	(setq sendmail-program "msmtp")))

(define-key mu4e-main-mode-map (kbd "S") 'karljoad/send-queued-mail)
(define-key mu4e-main-mode-map (kbd "f") 'karljoad/send-queued-mail)
(defun karljoad/send-queued-mail ()
  "Sends all mail currently stored in ~/.msmtpqueue/. Put output in *msmtp-runqueue Output* buffer."
  (interactive)
  ;; Now run the msmtp-runqueue.sh command, and put the output in a temporary buffer.
  (with-temp-buffer (async-shell-command "msmtp-runqueue.sh")))

;; Commented until I figure out how to make this work.
;; I want to print an additional command-context line in the main mu4e buffer.
;; (add-hook 'mu4e-main-mode
;; 	  (let ((buf (get-buffer mu4e-main-buffer-name)))
;; 	    (with-current-buffer buf
;; 	      (setq inhibit-read-only t)
;; 	      (insert
;; 	       (mu4e~main-action-str "\t[f]lush all queued mail and [S]end" 'karljoad/send-queued-mail))
;; 	      (setq inhibit-read-only nil))))

;; Use a sendmail program rather than sending directly from Emacs
(setq message-send-mail-function 'message-send-mail-with-sendmail)

;; Make msmtp infer the correct account to send from by the From: email address
(setq message-sendmail-extra-arguments '("--read-envelope-from"))

;; Don't add "-f username" to the msmtp command.
(setq message-sendmail-f-is-evil 't)

;; =============================================================================
;; My personal functions
;; =============================================================================
;; Closes all open buffers and jumps to mu4e mailbox
;; STILL A WORK IN PROGRESS!!
(defun karljoad/open-mu4e-context (context)
  "Closes all currently open buffers and opens CONTEXT in mu4e."
  (interactive "sContext:")
  (mu4e~headers-jump-to-maildir context))

;; Shamelessly stolen from Howard R. Schwarz's configuration.org file.
(defun karljoad/encrypt-responses ()
  "Encrypt the current message if it's a reply to another encrypted message."
  (let ((msg mu4e-compose-parent-message))
    (when (and msg (member 'encrypted (mu4e-message-field msg :flags)))
        (mml-secure-message-encrypt-pgpmime))))

(add-hook 'mu4e-compose-mode-hook 'karljoad/encrypt-responses)

(provide 'email-config)
;;; email-config.el ends here
