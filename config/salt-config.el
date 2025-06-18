;;; salt-config.el --- Configure Emacs to handle SaltStack configuration files -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package salt-mode
  :ensure t
  :defer t
  :hook (salt-mode . (lambda () (flyspell-mode 1))))

(provide 'salt-config)
;;; salt-config.el ends here
