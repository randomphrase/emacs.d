;;; mod-ui.el --- Look & feel -*- lexical-binding: t; -*-

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode t))

;; see https://emacsredux.com/blog/2023/03/16/setting-the-default-font-for-emacs/
(set-frame-font (format "%s-14" (cl-find-if (lambda (fn) (find-font (font-spec :name fn)))
					    '("Cascadia Code" "Menlo" "DejaVu Sans Mono" "Inconsolata")))
		nil t)

(setq split-height-threshold 100)

(use-package nerd-icons)

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  )

(use-package ef-themes
  :init (load-theme 'ef-night :no-confirm)
  ;; :custom
  ;; (ef-themes-common-palette-overrides '((bg-region bg-cyan-subtle)))
  )

(setq frame-title-format '("Emacs - " user-login-name "@" system-name ":%f"))

(use-package pulsar
  :ensure t
  :init
  (pulsar-global-mode 1)
  :hook
  (next-error . pulsar-pulse-line)
  (consult-after-jump . pulsar-recenter-top)
  (consult-after-jump . pulsar-reveal-entry)
  (imenu-after-jump-hook . pulsar-recenter-top)
  (imenu-after-jump-hook . pulsar-reveal-entry)
  )

(provide 'mod-ui)
;;; mod-ui.el ends here
