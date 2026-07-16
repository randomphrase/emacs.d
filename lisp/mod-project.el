;;; mod-project.el --- Project navigation/search -*- lexical-binding: t; -*-

(use-package savehist
  :straight nil
  :init
  (savehist-mode))

(defvar ar/project-test-history nil
  "Minibuffer history for `ar/project-test'.")

(defun ar/project-test ()
  "Run a test command from the project root (cf. `project-compile')."
  (interactive)
  (let* ((default-directory (project-root (project-current t)))
         (command (read-shell-command "Test command: "
                                      (car ar/project-test-history)
                                      'ar/project-test-history)))
    (compile command)))

(use-package project
  :straight nil
  :bind-keymap
  ("C-c p" . project-prefix-map)
  ("s-p" . project-prefix-map)
  :bind (("<f12>" . project-compile)
         ("C-<f12>" . ar/project-test)
         ("M-C-<f12>" . recompile)))

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
