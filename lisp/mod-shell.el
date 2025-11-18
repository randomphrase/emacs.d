;;; mod-shell.el --- Shells & terminals -*- lexical-binding: t; -*-

(use-package ansi-color
  :hook
  (compilation-filter . ansi-color-compilation-filter)
  :custom
  (ansi-color-for-compilation-mode t)
  )

;; (use-package vterm :commands (vterm))
;; (use-package eshell
;;   :ensure nil
;;   :hook (eshell-first-time-mode . (lambda () (setq eshell-scroll-to-bottom-on-input t))))

(provide 'mod-shell)
;;; mod-shell.el ends here
