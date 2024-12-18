;;; early-init.el --- Emacs 27+ pre-initialization config -*- lexical-binding: t -*-
;;; Commentary:
;; Emacs 27+ loads this file before (normally) calling
;; `package-initialize'.  We use this file to suppress that automatic
;; behaviour so that startup is consistent across Emacs versions.
;;; Code:
;;; -*- lexical-binding: t -*-

;; Disable package.el from start-up, because using elpaca
(setq package-enable-at-startup nil)

;; Increase the amount of heap memory Emacs is allowed to use before GC.
(defvar karljoad/gc-cons-threshold (* 32 1024 1024)
  "Karl's choice of the amount of memory used before GC is run.")
(defvar karljoad/gc-cons-percentage 0.8
  "Karl's choice of the percent memory used before GC is run.")
(defvar karljoad/message-log-max (* 16 1024)
  "Maximum number of messages to keep in *Messages* buffer.")
(setq-default gc-cons-threshold karljoad/gc-cons-threshold
              gc-cons-percentage karljoad/gc-cons-percentage
              message-log-max karljoad/message-log-max)

;; Make sure Emacs loads up newer config files, even if they aren't compiled
(setq load-prefer-newer t)

(scroll-bar-mode -1) ;; Remove scroll bar at side
(menu-bar-mode 1) ;; Keep the top menu-bar, with the drop-down menus
(tool-bar-mode -1) ;; Remove big icon tool-bar below the menu-bar.
(tooltip-mode -1) ;; On clickable text, remove tooltip pop-up. Use minibuffer.

;; Change the title of the frame when opened in GUI mode.
(setq-default frame-title-format
              '("%b@" (:eval (or (file-remote-p default-directory 'host)
                                 system-name))
                " - Emacs"))

;; So we can detect this having been loaded
(provide 'early-init)
;;; early-init.el ends here
