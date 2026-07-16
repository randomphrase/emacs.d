;;; mod-last.el --- Final touches -*- lexical-binding: t; -*-

;; ;; Optional per-host overrides (drop a file named after system-name)
;; (let* ((fname (format "lisp/%s.el" system-name))
;;        (path  (expand-file-name fname user-emacs-directory)))
;;   (when (file-readable-p path)
;;     (load path nil 'nomessage)))

;; As advised by the doco, this is initialized late in the startup sequence

(use-package envrc
  :if (executable-find "direnv")
  :config (envrc-global-mode)
  :bind ("C-c e" . envrc-command-map)
  )

;; save/restore desktop - do last to ensure all modes are ready
(require 'desktop) ; desktop-minor-mode-table isn't defined until loaded
;; Don't restore lsp's minor modes as inert flags: without a server behind
;; them a buffer looks LSP-enabled but isn't. lsp-deferred starts the real
;; thing when the buffer becomes visible.
(dolist (mode '(lsp-mode lsp-managed-mode lsp-diagnostics-mode
                lsp-completion-mode lsp-headerline-breadcrumb-mode
                lsp-modeline-code-actions-mode lsp-modeline-diagnostics-mode
                lsp-modeline-workspace-status-mode))
  (add-to-list 'desktop-minor-mode-table (list mode nil)))
(desktop-save-mode 1)

;; -- server

(server-mode 1)

(provide 'mod-last)
;;; mod-last.el ends here
