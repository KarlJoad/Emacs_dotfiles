;;; tramp-config.el --- Configure TRAMP, Transparent Remote Access, Multiple Protocol -*- lexical-binding: t -*-
;;; Commentary:
;;;
;;; When working with large files, there are 2 ways to move files from the
;;; remote machine an in-band mechanism and out-of-band mechanism. The in-band
;;; one sends the base64-encoded contents of the file over the SSH channel
;;; directly to Emacs and decodes them. The out-of-band transfer mecahnism uses
;;; an external program to copy the file around.
;;;
;;; Code:

(use-package tramp
  :ensure nil ; built-in
  :defer t
  :config
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))
  (connection-local-set-profiles
   '(:application tramp :protocol "scp")
   'remote-direct-async-process)
  (remove-hook 'compilation-mode-hook
               #'tramp-compile-disable-ssh-controlmaster-options)
  :custom
  ;; Disable file locking on remote files.
  ;; Beware if you are using multiple different Emacs sessions to open the same
  ;; file!
  (remote-file-name-inhibit-locks t)
  ;; Use SCP to directly copy from the remote machine to local for large edits.
  (tramp-use-scp-direct-remote-copying t)
  ;; Do not let auto-save-mode handle work on remote files, since that will just
  ;; slow down our working experience.
  (remote-file-name-inhibit-auto-save-visited t)
  ;; NOTE: rsync is the faster out-of-band transfer mechanism, but it currently
  ;; breaks remote shells, so SCP is used. Emacs 30.2 will have the fix for this.
  ;; The upper threshold in KiB where Emacs should switch from in-band transfer
  ;; to using out-of-band file transfer.
  (tramp-copy-size-limit (* 1024 1024))
  ;; Make copying with tramp slightly noisier.
  (tramp-verbose 2))


(provide 'tramp-config)
;;; tramp-config.el ends here
