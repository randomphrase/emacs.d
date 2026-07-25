;;; mod-git.el --- Git integration -*- lexical-binding: t; -*-

(use-package magit)

;; Teach project-switch-project (C-c p p) about Magit, deterministically.
;; Current Magit no longer registers this itself (magit-extras only
;; documents the snippet); older Magit did, but only from magit-extras,
;; which loads lazily -- so "m Magit" showed up in the menu just some
;; sessions. magit-project-status is autoloaded, so registering it here
;; forces no magit load.
(with-eval-after-load 'project
  (keymap-set project-prefix-map "m" #'magit-project-status)
  (add-to-list 'project-switch-commands '(magit-project-status "Magit") t))

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
