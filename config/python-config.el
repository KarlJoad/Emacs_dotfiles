;;; python-config.el --- Configure Python -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package python-mode
  :ensure nil ; built-in
  :defer t
  :config
  (add-to-list 'completion-ignored-extensions "__pycache__/")
  :custom
  (python-indent-offset 4))

(use-package python-ts-mode
  :ensure nil ; built-in
  :defer t
  :config
  (add-to-list 'completion-ignored-extensions "__pycache__/")
  :custom
  (python-indent-offset 4))

(provide 'python-config)
;;; python-config.el ends here
