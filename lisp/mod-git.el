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

;; Transient menus (Magit's command popups, and any other transient) default
;; to a side window at the bottom, which Emacs always makes full-frame-width.
;; On a wide, multi-window frame that popup lands far from the window you
;; invoked it in. Show it below the selected window instead, so it inherits
;; that window's width and stays local to where you're working.
(use-package transient
  :custom
  (transient-display-buffer-action
   '(display-buffer-below-selected
     (dedicated . t)
     (inhibit-same-window . t))))

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
