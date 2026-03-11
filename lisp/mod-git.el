;;; mod-git.el --- Git integration -*- lexical-binding: t; -*-

(use-package magit)

(use-package magit-delta
  :if (executable-find "delta")
  :hook (magit-mode . magit-delta-mode))

(use-package pinentry
  :ensure t
  :config
  (setq epa-pinentry-mode 'loopback)
  (pinentry-start))

(provide 'mod-git)
;;; mod-git.el ends here
