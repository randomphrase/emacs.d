;;; mod-git.el --- Git integration -*- lexical-binding: t; -*-

(use-package magit)

(use-package magit-delta
  :if (executable-find "delta")
  :hook (magit-mode . magit-delta-mode))


(provide 'mod-git)
;;; mod-git.el ends here
