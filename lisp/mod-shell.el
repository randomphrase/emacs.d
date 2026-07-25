;;; mod-shell.el --- Shells & terminals -*- lexical-binding: t; -*-

(use-package ansi-color
  :straight nil
  :hook
  (compilation-filter . ansi-color-compilation-filter)
  :custom
  (ansi-color-for-compilation-mode t)
  )


(provide 'mod-shell)
;;; mod-shell.el ends here
