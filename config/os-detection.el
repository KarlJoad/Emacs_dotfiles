;;; os-detection.el --- A library for detecting the current operating system -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun karljoad/this-system-this-linux-distro? (distro)
  "Return t or nil, depending on if the current OS is DISTRO."
  (interactive "MID of the distro (as found in /etc/os-release): ")
  (with-temp-buffer
    (insert (shell-command-to-string "cat /etc/os-release"))
    (goto-char 0)
    (condition-case nil
        (progn
          (when (not (equal (search-forward
                             (concat "ID=" distro) nil t)
                            nil))
            t)))))

(defun karljoad/is-nixos ()
  "Return t or nil, depending on if the current OS is NixOS."
  (karljoad/this-system-this-linux-distro? "nixos"))

(defun karljoad/is-guix-system ()
  "Return t or nil, depending on if the current OS is Guix System."
  (karljoad/this-system-this-linux-distro? "guix"))


(provide 'os-detection)
;;; os-detection.el ends here
