;;; lsp-config --- Settings for lsp-mode
;;; Commentary:
;;; Code:

(defun karljoad/close-eldoc-doc-buffer ()
  "Quit the Eldoc buffer window using `quit-window'.
This buries the buffer to the bottom of the buffer list and deletes the window."
  (interactive)
  (quit-window 'nil (get-buffer-window (eldoc-doc-buffer))))

(defun karljoad/eldoc-doc-buffer ()
  "Run Eldoc at symbol at point, switching to the Eldoc buffer in another window."
  (interactive)
  (switch-to-buffer-other-window (eldoc-doc-buffer)))

(use-package eldoc
  :straight (:type built-in)
  :ensure t
  :defer t
  :custom
  ;; Do not use multiline for documentation in the echo area (minibuffer)
  ;; FIXME: Do I actually want this? Perhaps just signatures in the minibuffer?
  (eldoc-echo-area-use-multiline-p 'nil)
  ;; Prefer to use the doc-buffer if it is already showing, rather than the
  ;; echo area (in the minibuffer).
  (eldoc-echo-area-prefer-doc-buffer 't))

(use-package eglot
  :straight (:type built-in)
  :ensure t
  :defer t
  :after (eldoc)
  :bind (("C-h ." . #'karljoad/eldoc-doc-buffer) ;; Override the default binding
         ("C-c h ." . #'karljoad/eldoc-doc-buffer)
         ;; Rebind eldoc to something else I use less often. eldoc will open
         ;; the buffer, but then not switch to it, just leaving it open to stare at.
         ("C-c h ?" . #'eldoc)
         ("C-h >" . #'karljoad/close-eldoc-doc-buffer))
  :hook (((c-mode c++-mode c-ts-mode c++-ts-mode) . eglot-ensure))
  :custom
  (eglot-autoshutdown t)
  ;; For performance, set this to a low number. When debugging, comment this out.
  ;; Setting to 0 means no messages/events are logged in the EGLOT events buffer.
  (eglot-events-buffer-size 0)
  ;; XRef look-ups can leave the project Eglot is running a server for
  (eglot-extend-to-xref t)
  (eglot-ignored-server-capabilities
   '(;; Disable LSP from providing highlighting, since I use treesitter-based or
     ;; Emacs' built-in regexp-based major modes for font-locking.
     :colorProvider
     :documentHighlightProvider
     :documentFormattingProvider
     :documentRangeFormattingProvider
     :documentOnTypeFormattingProvider
     :foldingRangeProvider))
  (eglot-stay-out-of '(yasnippet)))

;; (or (getenv "GUIX_ENVIRONMENT")
;;     (getenv "IN_NIX_SHELL"))

(provide 'lsp-config)
;;; lsp-config.el ends here
