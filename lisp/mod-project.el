;;; mod-project.el --- Project navigation/search -*- lexical-binding: t; -*-

(use-package savehist
  :init
  (savehist-mode))

(use-package projectile
  :after (savehist)
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)
	      ("<f12>" . projectile-compile-project)
	      ("C-<f12>" . projectile-test-project)
	      ("M-C-<f12>" . recompile)
	      ))

(use-package rg
  :bind ("C-c s" . rg-menu))

(recentf-mode 1)

;; ibuffer is more full featured than list-buffers
(use-package ibuffer-vc
  :hook (ibuffer . ibuffer-vc-set-filter-groups-by-vc-root)
  :bind ([remap list-buffers] . ibuffer)
  )

(provide 'mod-project)
;;; mod-project.el ends here
