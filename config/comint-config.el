;;; comint-config.el --- Configure comint -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package comint
  :ensure nil ; built-in
  :custom
  ;; Make sure comint's prompt is always read-only. I don't like accidentally
  ;; deleting part of the prompt, that is just confusing.
  (comint-prompt-read-only t)
  ;; Buffer's max size is 2KiB, default 1KiB is a bit small.
  (comint-buffer-maximum-size (* 2 1024))
  ;; Make sure that comint (and anything that inherits from comint, I'm looking
  ;; at you shell) ignored duplicated lines.
  (comint-input-ignoredups t)
  ;; Announce terminal as xterm-256color for proper color support
  (comint-terminfo-terminal "xterm-256color")
  ;; Don't force point to follow output (lets you read earlier output while new output arrives)
  (comint-move-point-for-output nil)
  ;; Don't force-scroll to bottom on every output
  (comint-scroll-show-maximum-output nil)
  ;; Auto-scroll to bottom when you type input
  (comint-scroll-to-bottom-on-input t))

(provide 'comint-config)
;;; comint-config.el ends here
