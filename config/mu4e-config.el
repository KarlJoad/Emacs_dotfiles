;;; mu4e-config.el --- This file configured mu4e for my email
;;; Commentary:
;;; Code:

;; (require 'mu4e)

;; Shamelessly stolen from Howard R. Schwarz's configuration.org file.
(defun karljoad/encrypt-responses ()
  "Encrypt the current message if it's a reply to another encrypted message."
  (let ((msg mu4e-compose-parent-message))
    (when (and msg (member 'encrypted (mu4e-message-field msg :flags)))
        (mml-secure-message-encrypt-pgpmime))))

(add-hook 'mu4e-compose-mode-hook 'hrs/encrypt-responses)

(provide 'mu4e-config)
;;; mu4e-config.el ends here
