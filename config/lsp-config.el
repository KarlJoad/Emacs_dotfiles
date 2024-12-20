;;; lsp-config --- Settings for lsp-mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; I require my markdown config because language servers can choose to return
;; their information/documentation as formatted markdown, which markdown-mode
;; can then nicely render for me.
(require 'markdown-config)

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
  :ensure nil ;; built-in
  :defer t
  :custom
  ;; Do not use multiline for documentation in the echo area (minibuffer)
  ;; FIXME: Do I actually want this? Perhaps just signatures in the minibuffer?
  (eldoc-echo-area-use-multiline-p 'nil)
  ;; Prefer to use the doc-buffer if it is already showing, rather than the
  ;; echo area (in the minibuffer).
  (eldoc-echo-area-prefer-doc-buffer 't)
  ;; Collects and displays all available documentation immediately, even if
  ;; multiple sources provide it. It concatenates the results.
  (eldoc-documentation-strategy #'eldoc-documentation-compose))

(use-package eglot
  :ensure nil ;; built-in
  :defer t
  :after (eldoc)
  :bind (:map eglot-mode-map
         ("C-h ." . #'karljoad/eldoc-doc-buffer) ;; Override the default binding
         ("C-c h ." . #'karljoad/eldoc-doc-buffer)
         ;; Rebind eldoc to something else I use less often. eldoc will open
         ;; the buffer, but then not switch to it, just leaving it open to stare at.
         ("C-c h ?" . #'eldoc)
         ("C-h >" . #'karljoad/close-eldoc-doc-buffer))
  :hook (((c-mode c++-mode c-ts-mode c++-ts-mode rust-mode rust-ts-mode
           scala-mode scala-ts-mode python-mode python-ts-mode) . eglot-ensure))
  :config
  (add-to-list
   'eglot-server-programs
   `((scala-ts-mode scala-mode) . ,(eglot-alternatives
                                    '("metals" "metals-emacs"))))
  :custom
  ;; When no buffers are connected to an LSP server, shut down the server and
  ;; eglot, to lighten the load on Emacs.
  (eglot-autoshutdown t)
  ;; For performance, set this to a low number. When debugging, comment this out.
  ;; Setting to 0 means no messages/events are logged in the EGLOT events buffer.
  ;; NOTE: In eglot 1.16, this variable was deprecated! If you still want to set
  ;; the events buffer size to 0, you need the following:
  ;; (setf (plist-get eglot-events-buffer-config :size) 0)
  (eglot-events-buffer-size 0)
  ;; For performance, set this to ignore. When debugging, comment this out.
  ;; fset-ing to ignore means no jsonrpc event are logged by Emacs.
  (fset #'jsonrpc--log-event #'ignore)
  ;; XRef look-ups can leave the project Eglot is running a server for
  (eglot-extend-to-xref t)
  ;; Wait some number of seconds before waiting for the connection to the LSP.
  ;; With nil, do not wait to connect at all, just try to connect immediately.
  (eglot-sync-connect nil)
  ;; Reduce the amount of time required for eglot to time-out LSP server
  ;; connection attempts.
  (eglot-connect-timeout 10)
  (eglot-ignored-server-capabilities
   '(;; Disable LSP from providing highlighting, since I use treesitter-based or
     ;; Emacs' built-in regexp-based major modes for font-locking.
     :colorProvider
     :documentHighlightProvider
     :foldingRangeProvider))
  (eglot-stay-out-of '(yasnippet))
  (eglot-workspace-configuration
   '((:pylsp . (:configurationSources ["flake8"]
                :plugins (:pycodestyle (:enabled :json-false)
                          :mccabe (:enabled :json-false)
                          :pyflakes (:enabled :json-false)
                          :flake8 (:enabled :json-false
                                            :maxLineLength 88)
                          :ruff (:enabled t :lineLength 88)
                          :pydocstyle (:enabled t :convention "numpy")
                          :yapf (:enabled :json-false)
                          :autopep8 (:enabled :json-false)
                          :black (:enabled t :line_length 88
                                           :cache_config t)))))))

;; (or (getenv "GUIX_ENVIRONMENT")
;;     (getenv "IN_NIX_SHELL"))

(provide 'lsp-config)
;;; lsp-config.el ends here
