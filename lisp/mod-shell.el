;;; mod-shell.el --- Shells & terminals -*- lexical-binding: t; -*-

(use-package ansi-color
  :straight nil
  :hook
  (compilation-filter . ansi-color-compilation-filter)
  :custom
  (ansi-color-for-compilation-mode t)
  )

(use-package tramp
  :straight nil
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  )

(provide 'mod-shell)
;;; mod-shell.el ends here
