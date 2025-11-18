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
(desktop-save-mode 1)

;; -- server

(server-mode 1)

(provide 'mod-last)
;;; mod-last.el ends here
