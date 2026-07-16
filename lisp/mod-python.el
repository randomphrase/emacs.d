;;; mod-python.el --- Python setup -*- lexical-binding: t; -*-

;; (use-package python :ensure nil
;;   :custom (python-indent-offset 4))

;; ;; Black formatter (optional)
;; (use-package blacken
;;   :hook (python-mode . blacken-mode)
;;   :custom (blacken-line-length 88))

;; LSP itself is handled by eglot (mod-programming); this pins the
;; server to pyright — eglot's default alternatives try pylsp first.
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("pyright-langserver" "--stdio"))))

(provide 'mod-python)
;;; mod-python.el ends here
