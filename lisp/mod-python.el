;;; mod-python.el --- Python setup -*- lexical-binding: t; -*-

;; (use-package python :ensure nil
;;   :custom (python-indent-offset 4))

;; ;; Black formatter (optional)
;; (use-package blacken
;;   :hook (python-mode . blacken-mode)
;;   :custom (blacken-line-length 88))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

;; ;; Eglot server config (pyright)
;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;                '(python-mode . ("pyright-langserver" "--stdio"))))

(provide 'mod-python)
;;; mod-python.el ends here
