;;; mod-python.el --- Python setup -*- lexical-binding: t; -*-

;; (use-package python :ensure nil
;;   :custom (python-indent-offset 4))

;; ;; Black formatter (optional)
;; (use-package blacken
;;   :hook (python-mode . blacken-mode)
;;   :custom (blacken-line-length 88))

;; LSP itself is handled by eglot (mod-programming); this pins the
;; server to pyright — eglot's default alternatives try pylsp first.
;; basedpyright preferred: it's the pip/uv-installable, self-contained
;; pyright distribution, so it can live inside each project's venv.
;; envrc makes exec-path buffer-local from the project's .envrc, so
;; these resolve to the venv's own server when eglot starts.
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               `((python-mode python-ts-mode)
                 . ,(eglot-alternatives
                     '(("basedpyright-langserver" "--stdio")
                       ("pyright-langserver" "--stdio"))))))

(defun ar/python-install-pyright ()
  "Install basedpyright into the current project's Python environment.
uv installs into the active venv ($VIRTUAL_ENV, e.g. set by envrc)
or the project's .venv; the venv must already exist."
  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (async-shell-command "uv pip install basedpyright")))

(defun ar/python-maybe-suggest-pyright ()
  "Hint when a Python buffer's environment offers no language server.
Runs from `find-file-hook' rather than the mode hook so envrc has
already applied the project environment."
  (when (and (derived-mode-p 'python-mode 'python-ts-mode)
             (not (executable-find "basedpyright-langserver"))
             (not (executable-find "pyright-langserver")))
    (message "No pyright in this environment; M-x ar/python-install-pyright adds basedpyright to the project venv")))
(add-hook 'find-file-hook #'ar/python-maybe-suggest-pyright)

(provide 'mod-python)
;;; mod-python.el ends here
