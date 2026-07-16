;;; mod-last.el --- Final touches -*- lexical-binding: t; -*-

;; ;; Optional per-host overrides (drop a file named after system-name)
;; (let* ((fname (format "lisp/%s.el" system-name))
;;        (path  (expand-file-name fname user-emacs-directory)))
;;   (when (file-readable-p path)
;;     (load path nil 'nomessage)))

;; As advised by the doco, this is initialized late in the startup sequence

(use-package envrc
  :if (executable-find "direnv")
  ;; :bind implies deferred loading, which would postpone
  ;; envrc-global-mode until the first C-c e; :demand forces the load.
  ;; This module runs last, satisfying envrc's "enable late" advice.
  :demand t
  :config (envrc-global-mode)
  :bind ("C-c e" . envrc-command-map)
  )

;; save/restore desktop - do last to ensure all modes are ready
;; (No lsp-mode-style minor-mode guards needed: eglot's managed mode is
;; buffer-internal and isn't persisted; eglot-ensure restarts servers.)
(desktop-save-mode 1)

;; -- server

(server-mode 1)

(provide 'mod-last)
;;; mod-last.el ends here
